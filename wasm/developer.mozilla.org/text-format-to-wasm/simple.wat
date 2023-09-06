(module 
 	(func $i (import "imports" "imported_func") (param i32))
 	(func (export "exported_func")
          i32.const 42
          call $i
          )
          (func (export "add1") (param $p1 i32) (result i32)
          local.get $p1
          i32.const 1
          i32.add
          )
)
