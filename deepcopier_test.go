package deepcopier

import (
	"database/sql"
	"fmt"
	"reflect"
	"strings"
	"testing"
	"time"

	"github.com/guregu/null"
	"github.com/lib/pq"
	"github.com/oleiade/reflections"
	"github.com/stretchr/testify/assert"
)

// -----------------------------------------------------------------------------
// Copy().To()
// -----------------------------------------------------------------------------

func TestCopyTo_Struct(t *testing.T) {
	var (
		data       = NewEntityData()
		entity     = NewEntity(data)
		entityCopy = &EntityCopy{}
		expected   = NewEntityCopy(data)
	)

	assert.Nil(t, Copy(entity).WithContext(data.MethodContext).To(entityCopy))

	table := getTable(t, expected, entityCopy, false)
	for _, tt := range table {
		assertEqual(t, tt.expected, tt.actual)
	}
}

func TestCopyTo_AnonymousStruct(t *testing.T) {
	var (
		data       = NewEntityData()
		entity     = NewEntity(data)
		entityCopy = &EntityCopyExtended{}
		expected   = NewEntityCopyExtended(data)
	)

	assert.Nil(t, Copy(entity).WithContext(data.MethodContext).To(entityCopy))

	table := getTable(t, expected, entityCopy, false)
	for _, tt := range table {
		assertEqual(t, tt.expected, tt.actual)
	}
}

func TestCopyTo_PtrToValue_String(t *testing.T) {
	var (
		st = "hello"
		v1 = struct{ Value *string }{Value: &st}
		v2 struct{ Value string }
	)

	assert.Nil(t, Copy(v1).To(&v2))
	assert.Equal(t, st, v2.Value)
}

func TestCopyTo_PtrToValue_Slice(t *testing.T) {
	var (
		slc = []string{"hello"}
		v1  = struct{ Value *[]string }{Value: &slc}
		v2  struct{ Value []string }
	)

	assert.Nil(t, Copy(v1).To(&v2))
	assert.Equal(t, slc, v2.Value)
}

func TestCopyTo_PtrToValue_Map(t *testing.T) {
	var (
		m  = map[string]interface{}{"error": false}
		v1 = struct{ Value *map[string]interface{} }{Value: &m}
		v2 struct{ Value map[string]interface{} }
	)

	assert.Nil(t, Copy(v1).To(&v2))
	assert.Equal(t, m, v2.Value)
}

func TestCopyTo_UnexportedField(t *testing.T) {
	var (
		st = "hello"
		v1 = struct{ value string }{value: st}
		v2 struct{ value string }
	)

	assert.Nil(t, Copy(v1).To(&v2))
	assert.Equal(t, "", v2.value) // Should not be copied (because bypassed)
}

func TestCopyTo_UnknownField(t *testing.T) {
	var (
		st = "hello"
		v1 = struct{ Value string }{Value: st}
		v2 struct{ Foo string }
	)

	assert.Nil(t, Copy(v1).To(&v2))
}

func TestCopyTo_Options_SkipField(t *testing.T) {
	var (
		st = "hello"
		v2 struct {
			Value string `deepcopier:"skip"`
		}
	)

	v1 := struct {
		Value string
	}{Value: st}

	assert.Nil(t, Copy(v1).To(&v2))
	assert.Equal(t, "", v2.Value) // Should not be copied (because bypassed)
}

func TestCopyTo_Options_SkipMethod(t *testing.T) {
	var (
		data       = NewEntityData()
		entity     = NewEntity(data)
		entityCopy = &EntityCopy{}
	)
	assert.Nil(t, Copy(entity).WithContext(data.MethodContext).To(entityCopy))
	assert.Empty(t, entityCopy.MethodSkipped)
}

func TestCopyTo_PtrToValue_AssignableTo(t *testing.T) {
	type Rel struct {
		ID int
	}

	type Src struct {
		ID  int
		Rel *Rel
	}

	type Dst struct {
		ID  int
		Rel string
	}

	var (
		rel = &Rel{ID: 2}
		src = &Src{ID: 1, Rel: rel}
		dst = &Dst{}
	)

	assert.Nil(t, Copy(src).To(dst))
	assert.Empty(t, dst.Rel)
}

func TestEmptyInterface(t *testing.T) {
	type Rel struct{ ID int }
	type Src struct{ Rel *Rel }
	type Dst struct{ Rel interface{} }

	type SrcForce struct {
		Rel *Rel `deepcopier:"force"`
	}

	type DstForce struct {
		Rel interface{} `deepcopier:"force"`
	}

	var (
		rel      = &Rel{ID: 2}
		src      = &Src{Rel: rel}
		srcForce = &SrcForce{Rel: rel}
	)

	//
	// Without force
	//

	dst := &Dst{}
	assert.Nil(t, Copy(src).To(dst))
	assert.Nil(t, dst.Rel)

	dst = &Dst{}
	assert.Nil(t, Copy(dst).From(src))
	assert.Nil(t, dst.Rel)

	//
	// With force
	//

	dstForce := &DstForce{}
	assert.Nil(t, Copy(src).To(dstForce))
	assert.Equal(t, src.Rel, dstForce.Rel)

	dstForce = &DstForce{}
	assert.Nil(t, Copy(dstForce).From(srcForce))
	assert.Equal(t, srcForce.Rel, dstForce.Rel)
}

// -----------------------------------------------------------------------------
// Copy().From()
// -----------------------------------------------------------------------------

func TestCopyFrom_Struct(t *testing.T) {
	var (
		data       = NewEntityData()
		entity     = &Entity{}
		entityCopy = NewEntityCopy(data)
		expected   = NewEntity(data)
	)

	assert.Nil(t, Copy(entity).From(entityCopy))

	table := getTable(t, expected, entity, true)
	for _, tt := range table {
		assertEqual(t, tt.expected, tt.actual)
	}
}

func TestCopyFrom_AnonymousStruct(t *testing.T) {
	var (
		data       = NewEntityData()
		entity     = &Entity{}
		entityCopy = NewEntityCopyExtended(data)
		expected   = NewEntity(data)
	)

	assert.Nil(t, Copy(entity).From(entityCopy))

	table := getTable(t, expected, entity, true)
	for _, tt := range table {
		assertEqual(t, tt.expected, tt.actual)
	}
}

func TestCopyFrom_PtrToValue_String(t *testing.T) {
	var (
		st = "hello"
		v1 = struct{ Value *string }{Value: &st}
		v2 struct{ Value string }
	)

	assert.Nil(t, Copy(&v2).From(v1))
	assert.Equal(t, st, v2.Value)
}

func TestCopyFrom_PtrToValue_Slice(t *testing.T) {
	var (
		slc = []string{"hello"}
		v1  = struct{ Value *[]string }{Value: &slc}
		v2  struct{ Value []string }
	)

	assert.Nil(t, Copy(&v2).From(v1))
	assert.Equal(t, slc, v2.Value)
}

func TestCopyFrom_PtrToValue_Map(t *testing.T) {
	var (
		m  = map[string]interface{}{"error": false}
		v1 = struct{ Value *map[string]interface{} }{Value: &m}
		v2 struct{ Value map[string]interface{} }
	)

	assert.Nil(t, Copy(&v2).From(v1))
	assert.Equal(t, m, v2.Value)
}

func TestCopyFrom_UnexportedField(t *testing.T) {
	var (
		st = "hello"
		v1 = struct{ value string }{value: st}
		v2 struct{ value string }
	)

	assert.Nil(t, Copy(&v2).From(v1))
	assert.Equal(t, "", v2.value) // Should not be copied (because bypassed)
}

func TestCopyFrom_UnknownField(t *testing.T) {
	var (
		st = "hello"
		v1 = struct{ Value string }{Value: st}
		v2 struct{ Foo string }
	)

	assert.Nil(t, Copy(&v2).From(v1))
}

func TestCopyFrom_Options_SkipField(t *testing.T) {
	var (
		st = "hello"
		v2 struct{ Value string }
	)

	v1 := struct {
		Value string `deepcopier:"skip"`
	}{Value: st}

	assert.Nil(t, Copy(&v2).From(v1))
	assert.Equal(t, "", v2.Value) // Should not be copied (because bypassed)
}

func TestCopyFrom_Options_SkipMethod(t *testing.T) {
	var (
		data       = NewEntityData()
		entity     = &Entity{}
		entityCopy = NewEntityCopy(data)
	)

	assert.Nil(t, Copy(entity).From(entityCopy))
	assert.NotEqual(t, entity.EntityCopyMethod, entityCopy.EntityCopyMethod())
}

func TestCopyFrom_PtrToValue_AssignableTo(t *testing.T) {
	type Rel struct {
		ID int
	}

	type Src struct {
		ID  int
		Rel *Rel
	}

	type Dst struct {
		ID  int
		Rel string
	}

	var (
		rel = &Rel{ID: 2}
		src = &Src{ID: 1, Rel: rel}
		dst = &Dst{}
	)

	assert.Nil(t, Copy(dst).From(src))
	assert.Empty(t, dst.Rel)
}

// -----------------------------------------------------------------------------
// Fixtures
// -----------------------------------------------------------------------------

type Entity struct {
	String              string
	StringPtr           *string
	Time                time.Time
	TimePtr             *time.Time
	IntField            int
	IntPtrField         *int
	Int64Field          int64
	Int64PtrField       *int64
	UIntField           uint
	UIntPtrField        *uint
	UInt64Field         uint64
	UInt64PtrField      *uint64
	Float64Field        float64
	Float64PtrField     *float64
	StringSliceField    []string
	StringSlicePtrField *[]string
	StringPtrSliceField []*string
	IntSliceField       []int
	IntSlicePtrField    *[]int
	NullStringField     null.String

	// Don't add me in fields mapping
	EntityCopyMethod string `deepcopier:"skip"`
}

func NewEntity(data *EntityData) *Entity {
	return &Entity{
		String:              data.String,
		StringPtr:           data.StringPtr,
		Time:                data.Time,
		TimePtr:             data.TimePtr,
		IntField:            data.Int,
		IntPtrField:         data.IntPtr,
		Int64Field:          data.Int64,
		Int64PtrField:       data.Int64Ptr,
		UIntField:           data.UInt,
		UIntPtrField:        data.UIntPtr,
		UInt64Field:         data.UInt64,
		UInt64PtrField:      data.UInt64Ptr,
		Float64Field:        data.Float64,
		Float64PtrField:     data.Float64Ptr,
		StringSliceField:    data.StringSlice,
		StringSlicePtrField: data.StringSlicePtr,
		IntSliceField:       data.IntSlice,
		IntSlicePtrField:    data.IntSlicePtr,
		NullStringField:     data.NullString,
	}
}

func (e *Entity) IntMethod() int                                    { return e.IntField }
func (e *Entity) Int64Method() int64                                { return e.Int64Field }
func (e *Entity) UIntMethod() uint                                  { return e.UIntField }
func (e *Entity) UInt64Method() uint64                              { return e.UInt64Field }
func (e *Entity) Float64Method() float64                            { return e.Float64Field }
func (e *Entity) MethodWithDifferentName() string                   { return e.String }
func (e *Entity) MethodWithContext(c map[string]interface{}) string { return c["version"].(string) }
func (e *Entity) MethodSkipped() int64                              { return 0 }

type RelatedEntity struct {
	String string
}

type EntityCopy struct {
	String            string
	StringPtr         *string
	Time              time.Time
	TimePtr           *time.Time
	Int               int         `deepcopier:"field:IntField"`
	IntPtr            *int        `deepcopier:"field:IntPtrField"`
	Int64             int64       `deepcopier:"field:Int64Field"`
	Int64Ptr          *int64      `deepcopier:"field:Int64PtrField"`
	UInt              uint        `deepcopier:"field:UIntField"`
	UIntPtr           *uint       `deepcopier:"field:UIntPtrField"`
	UInt64            uint64      `deepcopier:"field:UInt64Field"`
	UInt64Ptr         *uint64     `deepcopier:"field:UInt64PtrField"`
	Float64           float64     `deepcopier:"field:Float64Field"`
	Float64Ptr        *float64    `deepcopier:"field:Float64PtrField"`
	NullString        null.String `deepcopier:"field:NullStringField"`
	StringSlice       []string    `deepcopier:"field:StringSliceField"`
	StringSlicePtr    *[]string   `deepcopier:"field:StringSlicePtrField"`
	IntSlice          []int       `deepcopier:"field:IntSliceField"`
	IntSlicePtr       *[]int      `deepcopier:"field:IntSlicePtrField"`
	IntMethod         int
	Int64Method       int64
	UIntMethod        uint
	UInt64Method      uint64
	MethodWithContext string `deepcopier:"context"`
	SuperMethod       string `deepcopier:"field:MethodWithDifferentName"`

	// Don't add me in EntityFieldMapping
	MethodSkipped string `deepcopier:"skip"`
}

func (ec EntityCopy) EntityCopyMethod() string { return "entity copy method" }

func NewEntityCopy(data *EntityData) *EntityCopy {
	return &EntityCopy{
		String:            data.String,
		StringPtr:         data.StringPtr,
		Time:              data.Time,
		TimePtr:           data.TimePtr,
		Float64:           data.Float64,
		Float64Ptr:        data.Float64Ptr,
		Int:               data.Int,
		IntPtr:            data.IntPtr,
		Int64:             data.Int64,
		Int64Ptr:          data.Int64Ptr,
		UInt:              data.UInt,
		UIntPtr:           data.UIntPtr,
		UInt64:            data.UInt64,
		UInt64Ptr:         data.UInt64Ptr,
		StringSlice:       data.StringSlice,
		StringSlicePtr:    data.StringSlicePtr,
		IntSlice:          data.IntSlice,
		IntSlicePtr:       data.IntSlicePtr,
		NullString:        data.NullString,
		IntMethod:         data.Int,
		Int64Method:       data.Int64,
		UIntMethod:        data.UInt,
		UInt64Method:      data.UInt64,
		MethodWithContext: "1",
		SuperMethod:       "hello",
	}
}

type EntityCopyExtended struct {
	EntityCopy
}

func NewEntityCopyExtended(data *EntityData) *EntityCopyExtended {
	return &EntityCopyExtended{EntityCopy: *NewEntityCopy(data)}
}

type EntityData struct {
	Time           time.Time
	TimePtr        *time.Time
	String         string
	StringPtr      *string
	Int            int
	IntPtr         *int
	Int64          int64
	Int64Ptr       *int64
	UInt           uint
	UIntPtr        *uint
	UInt64         uint64
	UInt64Ptr      *uint64
	Float64        float64
	Float64Ptr     *float64
	StringSlice    []string
	StringSlicePtr *[]string
	StringPtrSlice []*string
	IntSlice       []int
	IntSlicePtr    *[]int
	IntPtrSlice    []*int
	Struct         RelatedEntity
	StructPtr      *RelatedEntity
	Map            map[string]interface{}
	MapPtr         *map[string]interface{}
	NullString     null.String
	PQNullTime     pq.NullTime
	SQLNullString  sql.NullString
	SQLNullInt64   sql.NullInt64
	MethodContext  map[string]interface{}
}

func NewEntityData() *EntityData {
	var (
		now              = time.Now()
		str              = "hello"
		integer          = 10
		integerPtr       = &integer
		integer64        = int64(64)
		integer64Ptr     = &integer64
		uinteger         = uint(10)
		uintegerPtr      = &uinteger
		uinteger64       = uint64(64)
		uinteger64Ptr    = &uinteger64
		f64              = float64(64)
		f64Ptr           = &f64
		stringSlice      = []string{"Chuck", "Norris"}
		stringSlicePtr   = &stringSlice
		stringPtrSlice   = []*string{&str}
		integerSlice     = []int{0, 8, 15}
		integerSlicePtr  = &integerSlice
		integerPtrSlice  = []*int{integerPtr}
		relatedEntity    = RelatedEntity{String: "I am the related entity"}
		relatedEntityPtr = &relatedEntity
		mp               = map[string]interface{}{"message": "ok", "valid": true}
		mpPtr            = &mp
		methodContext    = map[string]interface{}{"version": "1"}
	)

	return &EntityData{
		Time:           now,
		TimePtr:        &now,
		String:         str,
		StringPtr:      &str,
		Int:            integer,
		IntPtr:         integerPtr,
		Int64:          integer64,
		Int64Ptr:       integer64Ptr,
		UInt:           uinteger,
		UIntPtr:        uintegerPtr,
		UInt64:         uinteger64,
		UInt64Ptr:      uinteger64Ptr,
		Float64:        f64,
		Float64Ptr:     f64Ptr,
		StringSlice:    stringSlice,
		StringSlicePtr: stringSlicePtr,
		StringPtrSlice: stringPtrSlice,
		IntSlice:       integerSlice,
		IntSlicePtr:    integerSlicePtr,
		IntPtrSlice:    integerPtrSlice,
		Struct:         relatedEntity,
		StructPtr:      relatedEntityPtr,
		Map:            mp,
		MapPtr:         mpPtr,
		MethodContext:  methodContext,
	}
}

// -----------------------------------------------------------------------------
// Helpers
// -----------------------------------------------------------------------------

var EntityFieldMapping = map[string]string{
	"String":            "String",
	"StringPtr":         "StringPtr",
	"Time":              "Time",
	"TimePtr":           "TimePtr",
	"Int":               "IntField",
	"IntPtr":            "IntPtrField",
	"Int64":             "Int64Field",
	"Int64Ptr":          "Int64PtrField",
	"UInt":              "UIntField",
	"UIntPtr":           "UIntPtrField",
	"UInt64":            "UInt64Field",
	"UInt64Ptr":         "UInt64PtrField",
	"Float64":           "Float64Field",
	"Float64Ptr":        "Float64PtrField",
	"StringSlice":       "StringSliceField",
	"StringSlicePtr":    "StringSlicePtrField",
	"IntSlice":          "IntSliceField",
	"IntSlicePtr":       "IntSlicePtrField",
	"IntMethod":         "IntMethod",
	"Int64Method":       "Int64Method",
	"UIntMethod":        "UIntMethod",
	"UInt64Method":      "UInt64Method",
	"MethodWithContext": "MethodWithContext",
	"SuperMethod":       "SuperMethod",
}

type TableResult struct {
	expected, actual interface{}
}

func getTable(t *testing.T, expected interface{}, actual interface{}, reversed bool) []TableResult {
	var table []TableResult

	for k, v := range EntityFieldMapping {
		var (
			err    error
			result = TableResult{}
			field  = k
		)

		if reversed {
			field = v

			// If reversed, entity doesn't have method fields.
			if strings.Contains(field, "Method") {
				continue
			}
		}

		result.expected, err = reflections.GetField(expected, field)
		assert.Nil(t, err, reflect.ValueOf(expected).Type().String())

		result.actual, err = reflections.GetField(actual, field)
		assert.Nil(t, err, reflect.ValueOf(actual).Type().String())

		table = append(table, result)
	}

	return table
}

// assertEqual is a verbose version of assert.Equal()
func assertEqual(t *testing.T, in interface{}, out interface{}) {
	assert.Equal(t, in, out, fmt.Sprintf("%v not equal to %v", in, out))
}
