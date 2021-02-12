package deepcopier

import (
	"database/sql/driver"
	"fmt"
	"math"
	"reflect"
	"strings"
	"unsafe"
)

const (
	// TagName is the deepcopier struct tag name.
	TagName = "deepcopier"
	// FieldOptionName is the from field option name for struct tag.
	FieldOptionName = "field"
	// ContextOptionName is the context option name for struct tag.
	ContextOptionName = "context"
	// SkipOptionName is the skip option name for struct tag.
	SkipOptionName = "skip"
	// ForceOptionName is the skip option name for struct tag.
	ForceOptionName = "force"
)

type (
	// TagOptions is a map that contains extracted struct tag options.
	TagOptions map[string]string

	// Options are copier options.
	Options struct {
		// Context given to WithContext() method.
		Context map[string]interface{}
		// Reversed reverses struct tag checkings.
		Reversed bool
	}
)

// DeepCopier deep copies a struct to/from a struct.
type DeepCopier struct {
	dst interface{}
	src interface{}
	ctx map[string]interface{}
}

// Copy sets source or destination.
func Copy(src interface{}) *DeepCopier {
	return &DeepCopier{src: src}
}

// WithContext injects the given context into the builder instance.
func (dc *DeepCopier) WithContext(ctx map[string]interface{}) *DeepCopier {
	dc.ctx = ctx
	return dc
}

// To sets the destination.
func (dc *DeepCopier) To(dst interface{}) error {
	dc.dst = dst
	specs, err := preProcess(dc.dst, dc.src, Options{Context: dc.ctx})
	if err != nil {
		return err
	}
	fmt.Printf("specs: %+v\n", specs)
	executeSpec(specs, dc.dst, dc.src)
	return nil
	//return process(dc.dst, dc.src, Options{Context: dc.ctx})
}

// From sets the given the source as destination and destination as source.
func (dc *DeepCopier) From(src interface{}) error {
	dc.dst = dc.src
	dc.src = src
	specs, err := preProcess(dc.dst, dc.src, Options{Context: dc.ctx, Reversed: true})
	if err != nil {
		return err
	}
	fmt.Printf("specs: %+v\n", specs)
	executeSpec(specs, dc.dst, dc.src)
	return nil
}

type copyType byte

const (
	unknown                copyType = 0
	valuerReflectCopy      copyType = 1
	valuerReflectToPtrCopy copyType = 2
	directCopy             copyType = 3
	indirectCopy           copyType = 4
	ptrToInterfaceCopy     copyType = 5
)

type fieldCopySpec struct {
	size         uintptr
	srcOffset    uintptr
	srcTypePtr   uintptr
	srcFieldName string
	dstOffset    uintptr
	dstTypePtr   uintptr
	dstFieldName string
	copyType     copyType
	force        bool
}

type interfaceTuple struct {
	typePtr uintptr
	dataPtr uintptr
}

func executeSpec(specs []fieldCopySpec, dst interface{}, src interface{}) {
	srcInterfaceTuple := *(*interfaceTuple)(unsafe.Pointer(&src))
	directSrcMemoryHdr := reflect.SliceHeader{
		Data: srcInterfaceTuple.dataPtr,
		Len:  math.MaxInt64,
		Cap:  math.MaxInt64,
	}
	directSrcMemory := *(*[]byte)(unsafe.Pointer(&directSrcMemoryHdr))

	dstInterfaceTuple := *(*interfaceTuple)(unsafe.Pointer(&dst))
	directDstMemoryHdr := reflect.SliceHeader{
		Data: dstInterfaceTuple.dataPtr,
		Len:  math.MaxInt64,
		Cap:  math.MaxInt64,
	}
	directDstMemory := *(*[]byte)(unsafe.Pointer(&directDstMemoryHdr))

	var srcValuePtr *reflect.Value
	var dstValuePtr *reflect.Value

	var sysPtr uintptr
	pointerSize := unsafe.Sizeof(sysPtr)

	for _, copySpec := range specs {
		switch copySpec.copyType {
		case directCopy:
			srcMem := directSrcMemory[copySpec.srcOffset : copySpec.srcOffset+copySpec.size]
			dstMem := directDstMemory[copySpec.dstOffset : copySpec.dstOffset+copySpec.size]
			copy(dstMem, srcMem)
			continue
		case indirectCopy:
			srcPointerMem := directSrcMemory[copySpec.srcOffset : copySpec.srcOffset+pointerSize]
			srcPointer := (*(*[]uintptr)(unsafe.Pointer(&srcPointerMem)))[0]
			if srcPointer == 0 {
				continue
			}
			indirectSrcMemoryHdr := reflect.SliceHeader{
				Data: srcPointer,
				Len:  math.MaxInt64,
				Cap:  math.MaxInt64,
			}
			indirectSrcMemory := *(*[]byte)(unsafe.Pointer(&indirectSrcMemoryHdr))
			srcMem := indirectSrcMemory[0:copySpec.size]
			dstMem := directDstMemory[copySpec.dstOffset : copySpec.dstOffset+copySpec.size]
			copy(dstMem, srcMem)
			continue
		case ptrToInterfaceCopy:
			dstInterfaceMem := directDstMemory[copySpec.dstOffset:]
			dstInterfaceMemSlice := *(*[]uintptr)(unsafe.Pointer(&dstInterfaceMem))

			srcPtrMem := directSrcMemory[copySpec.srcOffset:]
			srcPtrMemSlice := *(*[]uintptr)(unsafe.Pointer(&srcPtrMem))

			dstInterfaceMemSlice[0] = copySpec.srcTypePtr
			dstInterfaceMemSlice[1] = srcPtrMemSlice[0]
			continue
		case valuerReflectCopy:
			if srcValuePtr == nil {
				srcValue := reflect.Indirect(reflect.ValueOf(src))
				srcValuePtr = &srcValue
			}
			srcFieldValue := srcValuePtr.FieldByName(copySpec.srcFieldName)
			if srcFieldValue == (reflect.Value{}) {
				continue
			}
			if dstValuePtr == nil {
				dstValue := reflect.Indirect(reflect.ValueOf(dst))
				dstValuePtr = &dstValue
			}
			dstFieldValue := dstValuePtr.FieldByName(copySpec.dstFieldName)
			if dstFieldValue == (reflect.Value{}) {
				continue
			}

			if srcFieldValue.Type().AssignableTo(dstFieldValue.Type()) {
				dstFieldValue.Set(srcFieldValue)
			}
			if copySpec.force {
				v, _ := srcFieldValue.Interface().(driver.Valuer).Value()
				if v == nil {
					continue
				}

				rv := reflect.ValueOf(v)
				if rv.Type().AssignableTo(dstFieldValue.Type()) {
					dstFieldValue.Set(rv)
				}
			}
			continue
		case valuerReflectToPtrCopy:
			if srcValuePtr == nil {
				srcValue := reflect.Indirect(reflect.ValueOf(src))
				srcValuePtr = &srcValue
			}
			srcFieldValue := srcValuePtr.FieldByName(copySpec.srcFieldName)
			if srcFieldValue == (reflect.Value{}) {
				continue
			}
			if dstValuePtr == nil {
				dstValue := reflect.Indirect(reflect.ValueOf(dst))
				dstValuePtr = &dstValue
			}
			dstFieldValue := dstValuePtr.FieldByName(copySpec.dstFieldName)
			if dstFieldValue == (reflect.Value{}) {
				continue
			}
			v, _ := srcFieldValue.Interface().(driver.Valuer).Value()
			if v == nil {
				continue
			}
			valueType := reflect.TypeOf(v)
			ptr := reflect.New(valueType)
			ptr.Elem().Set(reflect.ValueOf(v))

			if valueType.AssignableTo(dstFieldValue.Type().Elem()) {
				dstFieldValue.Set(ptr)
			}
			continue
		default:
			panic(fmt.Sprintf("unsupported copy spec type: %+v", copySpec))
		}
	}
}

func preProcess(dst interface{}, src interface{}, args ...Options) ([]fieldCopySpec, error) {
	var (
		options        = Options{}
		srcValue       = reflect.Indirect(reflect.ValueOf(src))
		dstValue       = reflect.Indirect(reflect.ValueOf(dst))
		srcFieldNames  = getFieldNames(src)
		srcMethodNames = getMethodNames(src)
	)

	if len(args) > 0 {
		options = args[0]
	}

	if !dstValue.CanAddr() {
		return nil, fmt.Errorf("destination %+v is unaddressable", dstValue.Interface())
	}

	fieldSpecs := make([]fieldCopySpec, 0, 16)

	for _, f := range srcFieldNames {
		var (
			srcFieldValue               = srcValue.FieldByName(f)
			srcFieldType, srcFieldFound = srcValue.Type().FieldByName(f)
			srcFieldName                = srcFieldType.Name
			dstFieldName                = srcFieldName
			tagOptions                  TagOptions
		)

		if !srcFieldFound {
			continue
		}

		if options.Reversed {
			tagOptions = getTagOptions(srcFieldType.Tag.Get(TagName))
			if v, ok := tagOptions[FieldOptionName]; ok && v != "" {
				dstFieldName = v
			}
		} else {
			if name, opts := getRelatedField(dst, srcFieldName); name != "" {
				dstFieldName, tagOptions = name, opts
			}
		}

		if _, ok := tagOptions[SkipOptionName]; ok {
			continue
		}

		var (
			dstFieldType, dstFieldFound = dstValue.Type().FieldByName(dstFieldName)
			dstFieldValue               = dstValue.FieldByName(dstFieldName)
		)

		if !dstFieldFound {
			continue
		}

		// Force option for empty interfaces and nullable types
		_, force := tagOptions[ForceOptionName]

		// Valuer -> ptr
		if isNullableType(srcFieldType.Type) && dstFieldValue.Kind() == reflect.Ptr && force {
			// We have same nullable type on both sides
			if srcFieldValue.Type().AssignableTo(dstFieldType.Type) {
				if srcFieldType.Type.Size() != dstFieldType.Type.Size() {
					panic(fmt.Sprintf(
						"srcFieldType.Type.Size() != dstFieldType.Type.Size(); "+
							"srcFieldType:%+v; srcFieldType.Type: %+v; srcFieldType.Type.Size: %+v;"+
							"dstFieldType:%+v; dstFieldType.Type: %+v; dstFieldType.Type.Size: %+v;",
						srcFieldType, srcFieldType.Type, srcFieldType.Type.Size(),
						dstFieldType, dstFieldType.Type, dstFieldType.Type.Size(),
					))
				}
				fieldSpecs = append(fieldSpecs, fieldCopySpec{
					size:      srcFieldType.Type.Size(),
					srcOffset: srcFieldType.Offset,
					dstOffset: dstFieldType.Offset,
					copyType:  directCopy,
				})
				continue
			}

			fieldSpecs = append(fieldSpecs, fieldCopySpec{
				srcFieldName: srcFieldName,
				dstFieldName: dstFieldName,
				copyType:     valuerReflectToPtrCopy,
			})
			continue
		}

		// Valuer -> value
		if isNullableType(srcFieldType.Type) {
			fieldSpecs = append(fieldSpecs, fieldCopySpec{
				srcFieldName: srcFieldName,
				dstFieldName: dstFieldName,
				copyType:     valuerReflectCopy,
				force:        force,
			})
			continue
		}

		if dstFieldValue.Kind() == reflect.Interface {
			if force {
				if srcFieldValue.Kind() == reflect.Interface {
					fieldSpecs = append(fieldSpecs, fieldCopySpec{
						size:      srcFieldType.Type.Size(),
						srcOffset: srcFieldType.Offset,
						dstOffset: dstFieldType.Offset,
						copyType:  directCopy,
					})
					continue
				}
				if srcFieldValue.Kind() == reflect.Ptr {
					fieldSpecs = append(fieldSpecs, fieldCopySpec{
						size:       srcFieldType.Type.Size(),
						srcOffset:  srcFieldType.Offset,
						srcTypePtr: extractTypePtr(srcFieldValue),
						dstOffset:  dstFieldType.Offset,
						copyType:   ptrToInterfaceCopy,
					})
					continue
				}
				panic("unsupported src type for interface destination")
			}
			continue
		}

		// Ptr -> Value
		if srcFieldType.Type.Kind() == reflect.Ptr && !srcFieldValue.IsNil() && dstFieldType.Type.Kind() != reflect.Ptr {
			indirect := reflect.Indirect(srcFieldValue)
			if indirect.Type().AssignableTo(dstFieldType.Type) {
				fieldSpecs = append(fieldSpecs, fieldCopySpec{
					size:      indirect.Type().Size(),
					srcOffset: srcFieldType.Offset,
					dstOffset: dstFieldType.Offset,
					copyType:  indirectCopy,
				})
				continue
			}
		}

		// Other types
		if srcFieldType.Type.AssignableTo(dstFieldType.Type) {
			if srcFieldType.Type.Size() != dstFieldType.Type.Size() {
				panic(fmt.Sprintf(
					"srcFieldType.Type.Size() != dstFieldType.Type.Size(); "+
						"srcFieldType:%+v; srcFieldType.Type: %+v; srcFieldType.Type.Size: %+v;"+
						"dstFieldType:%+v; dstFieldType.Type: %+v; dstFieldType.Type.Size: %+v;",
					srcFieldType, srcFieldType.Type, srcFieldType.Type.Size(),
					dstFieldType, dstFieldType.Type, dstFieldType.Type.Size(),
				))
			}
			fieldSpecs = append(fieldSpecs, fieldCopySpec{
				size:      srcFieldType.Type.Size(),
				srcOffset: srcFieldType.Offset,
				dstOffset: dstFieldType.Offset,
				copyType:  directCopy,
			})
		}
	}

	for _, m := range srcMethodNames {
		name, opts := getRelatedField(dst, m)
		if name == "" {
			continue
		}

		if _, ok := opts[SkipOptionName]; ok {
			continue
		}

		method := reflect.ValueOf(src).MethodByName(m)
		if !method.IsValid() {
			return nil, fmt.Errorf("method %s is invalid", m)
		}

		var (
			dstFieldType, _ = dstValue.Type().FieldByName(name)
			dstFieldValue   = dstValue.FieldByName(name)
			_, withContext  = opts[ContextOptionName]
			_, force        = opts[ForceOptionName]
		)

		args := []reflect.Value{}
		if withContext {
			args = []reflect.Value{reflect.ValueOf(options.Context)}
		}

		var (
			result          = method.Call(args)[0]
			resultInterface = result.Interface()
			resultValue     = reflect.ValueOf(resultInterface)
			resultType      = resultValue.Type()
		)

		// Value -> Ptr
		if dstFieldValue.Kind() == reflect.Ptr && force {
			ptr := reflect.New(resultType)
			panic("#365")
			ptr.Elem().Set(resultValue)

			if ptr.Type().AssignableTo(dstFieldType.Type) {
				panic("#369")
				dstFieldValue.Set(ptr)
			}

			continue
		}

		// Ptr -> value
		if resultValue.Kind() == reflect.Ptr && force {
			if resultValue.Elem().Type().AssignableTo(dstFieldType.Type) {
				panic("#379")
				dstFieldValue.Set(resultValue.Elem())
			}

			continue
		}

		if resultType.AssignableTo(dstFieldType.Type) && result.IsValid() {
			panic("#387")
			dstFieldValue.Set(result)
		}
	}

	return fieldSpecs, nil
}

func extractTypePtr(srcFieldValue reflect.Value) uintptr {
	dummyValue := struct {
		Field interface{}
	}{
		Field: srcFieldValue.Interface(),
	}
	dummyFiled := reflect.ValueOf(dummyValue).FieldByName("Field")
	typePtr := dummyFiled.InterfaceData()[0]
	return typePtr
}

// process handles copy.
func process(dst interface{}, src interface{}, args ...Options) error {
	var (
		options        = Options{}
		srcValue       = reflect.Indirect(reflect.ValueOf(src))
		dstValue       = reflect.Indirect(reflect.ValueOf(dst))
		srcFieldNames  = getFieldNames(src)
		srcMethodNames = getMethodNames(src)
	)

	if len(args) > 0 {
		options = args[0]
	}

	if !dstValue.CanAddr() {
		return fmt.Errorf("destination %+v is unaddressable", dstValue.Interface())
	}

	for _, f := range srcFieldNames {
		var (
			srcFieldValue               = srcValue.FieldByName(f)
			srcFieldType, srcFieldFound = srcValue.Type().FieldByName(f)
			srcFieldName                = srcFieldType.Name
			dstFieldName                = srcFieldName
			tagOptions                  TagOptions
		)

		if !srcFieldFound {
			continue
		}

		if options.Reversed {
			tagOptions = getTagOptions(srcFieldType.Tag.Get(TagName))
			if v, ok := tagOptions[FieldOptionName]; ok && v != "" {
				dstFieldName = v
			}
		} else {
			if name, opts := getRelatedField(dst, srcFieldName); name != "" {
				dstFieldName, tagOptions = name, opts
			}
		}

		if _, ok := tagOptions[SkipOptionName]; ok {
			continue
		}

		var (
			dstFieldType, dstFieldFound = dstValue.Type().FieldByName(dstFieldName)
			dstFieldValue               = dstValue.FieldByName(dstFieldName)
		)

		if !dstFieldFound {
			continue
		}

		// Force option for empty interfaces and nullable types
		_, force := tagOptions[ForceOptionName]

		// Valuer -> ptr
		if isNullableType(srcFieldType.Type) && dstFieldValue.Kind() == reflect.Ptr && force {
			// We have same nullable type on both sides
			if srcFieldValue.Type().AssignableTo(dstFieldType.Type) {
				dstFieldValue.Set(srcFieldValue)
				continue
			}

			v, _ := srcFieldValue.Interface().(driver.Valuer).Value()
			if v == nil {
				continue
			}

			valueType := reflect.TypeOf(v)

			ptr := reflect.New(valueType)
			ptr.Elem().Set(reflect.ValueOf(v))

			if valueType.AssignableTo(dstFieldType.Type.Elem()) {
				dstFieldValue.Set(ptr)
			}

			continue
		}

		// Valuer -> value
		if isNullableType(srcFieldType.Type) {
			// We have same nullable type on both sides
			if srcFieldValue.Type().AssignableTo(dstFieldType.Type) {
				dstFieldValue.Set(srcFieldValue)
				continue
			}

			if force {
				v, _ := srcFieldValue.Interface().(driver.Valuer).Value()
				if v == nil {
					continue
				}

				rv := reflect.ValueOf(v)
				if rv.Type().AssignableTo(dstFieldType.Type) {
					dstFieldValue.Set(rv)
				}
			}

			continue
		}

		if dstFieldValue.Kind() == reflect.Interface {
			if force {
				dstFieldValue.Set(srcFieldValue)
			}
			continue
		}

		// Ptr -> Value
		if srcFieldType.Type.Kind() == reflect.Ptr && !srcFieldValue.IsNil() && dstFieldType.Type.Kind() != reflect.Ptr {
			indirect := reflect.Indirect(srcFieldValue)

			if indirect.Type().AssignableTo(dstFieldType.Type) {
				dstFieldValue.Set(indirect)
				continue
			}
		}

		// Other types
		if srcFieldType.Type.AssignableTo(dstFieldType.Type) {
			dstFieldValue.Set(srcFieldValue)
		}
	}

	for _, m := range srcMethodNames {
		name, opts := getRelatedField(dst, m)
		if name == "" {
			continue
		}

		if _, ok := opts[SkipOptionName]; ok {
			continue
		}

		method := reflect.ValueOf(src).MethodByName(m)
		if !method.IsValid() {
			return fmt.Errorf("method %s is invalid", m)
		}

		var (
			dstFieldType, _ = dstValue.Type().FieldByName(name)
			dstFieldValue   = dstValue.FieldByName(name)
			_, withContext  = opts[ContextOptionName]
			_, force        = opts[ForceOptionName]
		)

		args := []reflect.Value{}
		if withContext {
			args = []reflect.Value{reflect.ValueOf(options.Context)}
		}

		var (
			result          = method.Call(args)[0]
			resultInterface = result.Interface()
			resultValue     = reflect.ValueOf(resultInterface)
			resultType      = resultValue.Type()
		)

		// Value -> Ptr
		if dstFieldValue.Kind() == reflect.Ptr && force {
			ptr := reflect.New(resultType)
			ptr.Elem().Set(resultValue)

			if ptr.Type().AssignableTo(dstFieldType.Type) {
				dstFieldValue.Set(ptr)
			}

			continue
		}

		// Ptr -> value
		if resultValue.Kind() == reflect.Ptr && force {
			if resultValue.Elem().Type().AssignableTo(dstFieldType.Type) {
				dstFieldValue.Set(resultValue.Elem())
			}

			continue
		}

		if resultType.AssignableTo(dstFieldType.Type) && result.IsValid() {
			dstFieldValue.Set(result)
		}
	}

	return nil
}

// getTagOptions parses deepcopier tag field and returns options.
func getTagOptions(value string) TagOptions {
	options := TagOptions{}

	for _, opt := range strings.Split(value, ";") {
		o := strings.Split(opt, ":")

		// deepcopier:"keyword; without; value;"
		if len(o) == 1 {
			options[o[0]] = ""
		}

		// deepcopier:"key:value; anotherkey:anothervalue"
		if len(o) == 2 {
			options[strings.TrimSpace(o[0])] = strings.TrimSpace(o[1])
		}
	}

	return options
}

// getRelatedField returns first matching field.
func getRelatedField(instance interface{}, name string) (string, TagOptions) {
	var (
		value      = reflect.Indirect(reflect.ValueOf(instance))
		fieldName  string
		tagOptions TagOptions
	)

	for i := 0; i < value.NumField(); i++ {
		var (
			vField     = value.Field(i)
			tField     = value.Type().Field(i)
			tagOptions = getTagOptions(tField.Tag.Get(TagName))
		)

		if tField.Type.Kind() == reflect.Struct && tField.Anonymous {
			if n, o := getRelatedField(vField.Interface(), name); n != "" {
				return n, o
			}
		}

		if v, ok := tagOptions[FieldOptionName]; ok && v == name {
			return tField.Name, tagOptions
		}

		if tField.Name == name {
			return tField.Name, tagOptions
		}
	}

	return fieldName, tagOptions
}

// getMethodNames returns instance's method names.
func getMethodNames(instance interface{}) []string {
	var methods []string

	t := reflect.TypeOf(instance)
	for i := 0; i < t.NumMethod(); i++ {
		methods = append(methods, t.Method(i).Name)
	}

	return methods
}

// getFieldNames returns instance's field names.
func getFieldNames(instance interface{}) []string {
	var (
		fields []string
		v      = reflect.Indirect(reflect.ValueOf(instance))
		t      = v.Type()
	)

	if t.Kind() != reflect.Struct {
		return nil
	}

	for i := 0; i < v.NumField(); i++ {
		var (
			vField = v.Field(i)
			tField = v.Type().Field(i)
		)

		// Is exportable?
		if tField.PkgPath != "" {
			continue
		}

		if tField.Type.Kind() == reflect.Struct && tField.Anonymous {
			fields = append(fields, getFieldNames(vField.Interface())...)
			continue
		}

		fields = append(fields, tField.Name)
	}

	return fields
}

// isNullableType returns true if the given type is a nullable one.
func isNullableType(t reflect.Type) bool {
	return t.ConvertibleTo(reflect.TypeOf((*driver.Valuer)(nil)).Elem())
}
