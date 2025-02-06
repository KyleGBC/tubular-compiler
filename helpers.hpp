#pragma once
#include "Types.hpp"
#include <optional>
#include <utility>

// the info needed to call this function in the Tubular frontend
struct CallingInfo
{
	std::string name;
	std::vector<Type> params;
	Type return_type;
	CallingInfo(std::string name, std::vector<Type> params, Type return_type) : name(name), params(params), return_type(return_type) {};
};

struct IntrinsicFunction
{
	std::string wasm;
	std::optional<CallingInfo> calling_info;
};

IntrinsicFunction size = IntrinsicFunction{
	R"( 	(func $size (param $1 i32) (result i32)
		(local $2 i32)
		(block $_str_size_exit (result i32)
			(i32.const 0)
			(local.set $2)
			(block $block_0
				(loop $loop_0
					(local.get $1)
					(local.get $2)
					(i32.add)
					(i32.load8_u)
					(i32.const 0)
					(i32.ne)
					(if
						(then)
						(else
							(br $block_0)
						)
					)
						(local.get $2)
						(i32.const 1)
						(i32.add)
						(local.set $2)
					(br $loop_0)
				)
			)
			(local.get $2)
			(br $_str_size_exit)
		)
	))",
	{
		{"size", {Type::STRING}, Type::INT},
	}
};

IntrinsicFunction _alloc_str = IntrinsicFunction{
	R"(	(func $_alloc_str (param $size i32) (result i32)
		(local $null_pos i32) ;; Local variable to place null terminator.");
		(global.get $free_mem)
		(global.get $free_mem)
		(local.get $size)
		(i32.add)
		(local.set $null_pos)
		(i32.store8 (local.get $null_pos) (i32.const 0))
		(i32.add (i32.const 1) (local.get $null_pos))
		(global.set $free_mem)
	))",
	{
		{"_alloc_str", {Type::INT}, Type::STRING}
	}
};

IntrinsicFunction _memcpy = IntrinsicFunction{
	R"(	(func $_memcpy (param $src i32) (param $dest i32) (param $count i32) (result i32)
		(local $i i32)

		(i32.const 0)
		(local.set $i)
		(block $_memcpy_loop_break
			(loop $_memcpy_loop
				(local.get $i)
				(local.get $count)
				(i32.lt_s)
				(if
					(then)
					(else
						(local.get $dest)
						(return)
					) 
				)

				(local.get $dest)
				(local.get $i)
				(i32.add)

				(local.get $src)
				(local.get $i)
				(i32.add)

				(i32.load8_u)
				(i32.store8)

				(local.get $i)
				(i32.const 1)
				(i32.add)
				(local.set $i)
				(br $_memcpy_loop)
			)
		)
		(local.get $i)
		(return)
	))"
};

IntrinsicFunction _str_cat = IntrinsicFunction{
	R"(	(func $_str_cat (param $lhs i32) (param $rhs i32) (result i32)
		(local $new i32)
		(local $lhs_size i32)
		(local $rhs_size i32)

		(block $_str_cat_exit (result i32)
			(local.get $lhs)
			(call $size)
			(local.set $lhs_size)

			(local.get $rhs)
			(call $size)
			(local.set $rhs_size)

			(local.get $lhs_size)
			(local.get $rhs_size)
			(i32.add)
			(call $_alloc_str)
			(local.set $new)

			(local.get $lhs)
			(local.get $new)
			(local.get $lhs_size)
			(call $_memcpy)
			(drop)

			(local.get $rhs)
			(local.get $new)
			(local.get $lhs_size)
			(i32.add)
			(local.get $rhs_size)
			(call $_memcpy)
			(drop)

			(local.get $new) 
			(br $_str_cat_exit)
		)
	))"
};

IntrinsicFunction _str_repeat = IntrinsicFunction{
	R"(	(func $_str_repeat (param $s i32) (param $n i32) (result i32)
		(local $i i32)
		(local $sz i32)
		(local $new i32)

		(i32.const 0)
		(local.set $i)

		(local.get $s)
		(call $size)
		(local.set $sz)

		(local.get $sz)
		(local.get $n)
		(i32.mul)
		(call $_alloc_str)
		(local.set $new)

		(block $_str_repeat_loop_break
			(loop $_str_repeat_loop
				(local.get $i)
				(local.get $n)
				(i32.lt_s)
				(if
					(then)
					(else
						(local.get $new)
						(return)
					) 
				)
				;; Push the source string to the stack
				(local.get $s)

				;; Compute new[size(s) * i]
				(local.get $sz)
				(local.get $i)
				(i32.mul)
				(local.get $new)
				(i32.add)

				;; memcpy
				(local.get $sz)
				(call $_memcpy)
				
				(local.get $i)
				(i32.const 1)
				(i32.add)
				(local.set $i)
				(br $_str_repeat_loop)
			)
		)	
		(i32.const 0)
		(return)
	))"
};

IntrinsicFunction _str_eq = IntrinsicFunction{
	R"(	(func $_str_eq (param $s1 i32) (param $s2 i32) (result i32)
		(local $sz i32)
		(local $i i32)

		(i32.const 0)
		(local.set $i)

		(local.get $s1)
		(call $size)
		(local.set $sz)

		(local.get $s2)
		(call $size)
		(local.get $sz)
		(i32.ne)
		(if
			(then
				(i32.const 0)
				(return)
			)
			(else)
		)

		(block $_str_eq_loop_break
			(loop $_str_eq_loop
				(local.get $i)
				(local.get $sz)
				(i32.lt_s)
				(if
					(then)
					(else
						(i32.const 0)
						(return)
					) 
				)

				(local.get $s1)
				(local.get $i)
				(i32.add)
				(i32.load8_u)

				(local.get $s2)
				(local.get $i)
				(i32.add)
				(i32.load8_u)

				(i32.ne)
				(if
					(then
						(i32.const 0)
						(return)
					)
					(else)
				)	


				(local.get $i)
				(i32.const 1)
				(i32.add)
				(local.set $i)
				(br $_str_eq_loop)
			)
		)
		(i32.const 1)
	))"
};

IntrinsicFunction _i32_dup = IntrinsicFunction{
	R"(	(func $_i32_dup (param $n i32) (result i32 i32)
		(local.get $n)
		(local.get $n)
	))"
};

const IntrinsicFunction AVAILABLE_INTRINSICS[] = {size, _alloc_str, _memcpy, _str_cat, _str_repeat, _str_eq, _i32_dup};
