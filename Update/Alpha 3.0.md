### Update Alpha: 3.0v
Update made to fix so much bugs are found. Implement so many new functions and memory management functions & Start working on error handing and new features could come to Vix like "interpeter VM" plan. More error handling and memory safe. Now **FFI** require **unsafe blocks** and std library will be start developed with **Vix** it self. Compiler clean up


#### | Interpeter VM:
Vix is knowen as compiled language compile into C IR that uses **clang** for compiling to machine code But this so slow. If you want example fix bugs/errors fast without waiting for compilion that take so long every time you can use **Vix interpeted VM** it don't generate compiled application like **.exe** it does just run your code inside a VM ( interpeted ), using LLVM llv and clang make this possible. This Implemtion is still on planning and prototypes and there is nothing tell that its 100% coming. If you wanna help us in making this possible tell us in "issues".

#### Compiler updates:
Compiler every compiled update like "3.0v"/"2.0v" have a big clean up from cleaning the mess, deleting unsable functions etc...
- Compiler Updates:
    - Removing dead functions/codes that not usable anymore:
    ```rust
        pub fn alignment(&self, arch: &ArchConfig) -> usize {}
    ```
- Variables now being handled differently:
    - Before was all variables be a normal **variable** example:
        ```c
            struct SliceStr {
                
            } SliceStr_int_str
        ```     

#### Bugs:
In this update not much bugs found but of all bugs found is the function param generated IR is wrong:
- Example:
    ```ruby
        func example(i: int[3])
        end

        example([10, 50, 30])
    ```
- Was generating:
    ```c
        int32_t example(int32_t var_i)
    ```
IS wrong. The bug has been fixed by adding new type of type handling for:
- Array
- String
- Char
- Const String
- Any
Using the new **Slice_Type** struct in C IR:
```c
    typedef struct {
        char* ptr;
        size_t len;
    } Slice_char;
```
Is correct IR generating and give us a good balance with:
- More safety
- High perfomrance

#### New Error handling:
After 3 votes on our [discord server](https://discord.gg/VgxSzj4fBQ) between **try/catch** or **?** selected is **?**.


#### New imeplemtions:
So much new imeplemetions in this update:
- Fixed Array:
```ruby
    create mut temp: Array[uint8] = [0; 1024]
```
So useful for system programming, and being used in std libraries
- HashMap:
```ruby
    mut friends = {
        ["Example:"] = (15, -10),
        ["YourFriend"] = (20, 104)
    }
```

- New build in functions:
    - ```.as_bytes()``` make your data as bytes
    - ```.as_ptr()``` make your data a ptr
    - ```.as_mut_ptr()``` make your dad mutable ptr

- New Error & memory management:
```ruby
    # this function have "Result" can return "ok" or "err".
    func some_function(input: Option[str]): Result[T: str, E: str]
        input.unwrap_or(
            return Err("Failed to unwrap string aka: string is a none")
        )

        Ok(input)
    end

    # When will sent "None" will return a error
    if Ok(output) = some_funtion(None) then

    end

    print("No! function is not okay, it's returning a error")
    if Err(err) = some_function(None) then
        print("We got it the error is", err)
    end
```
- But function can return a **None** can fix this by:
```ruby
    func other_function(): Result[Option[int32], str]
        return Ok(None)
    end

    if Some(output) = other_function() then
        # ...
    end
```
- If you want just to get the output:
```ruby
    func example_function(): int32
        return 10
    end

    if let(output) = example_function() then
        print(output)
    end
```
The full update added:
- let()
- Some()
- Err()
- Ok()
for the "if" statment for more safety and better memory maangement. You can ignore the error or output using "_" example:
```ruby
    if Ok(_) = example_function() then
        print("function is successful")
    end

    if Err(_) = example_function() then
        print("function just failed")
    end
```

#### Deep Explanation & Examples:
- ```.as_bytes()``` aka: Converting to Bytes Slices:
    Vix use them for Converts string data into a slice of bytes ```(&[u8])```, providing a view of the underlying UTF-8 encoded data.
    It's so useful for Network programming bc require to sent a raw bytes and I/O perations that work with raw bytes too. It's so used for cryptographic (aka: 'hashing' 'encryption'). And much more.
    - Example:
    ```ruby
        create txt = "Hello"
        bytes = txt.as_bytes() # &[u8] = [72, 101, 108, 108, 111]
    ```

    - String in Vix are always [UTF8](). ```as_bytes``` gives you:
        - safe,
        - zero cost view of that [UTF8]() data
        - Non copying and overhead. you're just viewing the same memory with a different type.
        - Return immutable reference aka('&[u8]')

- ```.as_ptr()``` aka: Getting a Raw Immutable Pointer
    Vix use them for returns a raw pointer aka('*const T') to the first element of the data, giving you direct memory accese. Comman usage in: FFI with [C/C++]() Passing data to system calls, Low level memory operations. And it's Performance critical code where you need to bypass bounds checking.

    - Example:
    ```ruby
        create numbers = [1, 2, 3, 4, 5]
        create ptr: *const int32 = numbers.as_ptr();

        unsafe:
            print("First element:", *ptr)
        end
    ```
    - Return ```*const T``` aka('immutable raw pointer')
    - The pointer points to first element in the data
    - The pointer is only valid while the original data exists.
    ```Warnring:``` 
        - It require Unsafe blocks to run and use
        - If the pointer is empty, you'll get a ```non null dangling pointer``` That can easily crash your program or corrupt your memory!
        - Make sure that you ensure the data isn't moved or dropped
        - Violating aliasing rules aka('multiple mutable accesses') is undefined behavior
- ```as_mut_ptr```:
    Vix use them for Returns a raw mutable pointer aka('*mut T') allowing direct memory modification without Rust's borrow checker oversight. Like [as_ptr()]() they have mustly same usage in: FFI with [C](), Implementing custom data structures; Performance critical code with manual memory management and Interfacing with hardware or memory mapped I/O and much more...
    - Example:
    ```ruby
        create mut numbers = [1, 2, 3, 4, 5]
        create ptr: *mut int32 = numbers.as_mut_ptr();

        unsafe:
            *ptr = 100
            *ptr.slot(1) = 200
        end

        print(numbers)
    ```
    - Returns *mut T aka('mutable raw pointer')
    - Requires mutable access to the original data aka('.as_mut_ptr() needs &mut self')
    ```Warning```:
        - More dangerous than .as_ptr() because it allows modification
        - No aliasing: Don't create multiple mutable references to the same memory
        - Bounds checking: You're responsible for staying within valid memory

#### Comparison Table:
---
| Function | Returns |  Mutablility | Safety | Example Common Use Case |
|----------|----------|-------------|--------|-------------------------|
| ```.as_bytes()```  | ```&[u8]```  | Immutable   | Safe   | String → bytes conversion
| ```.as_ptr()```  | ```*const T```  | Immutable   | Unsafe | FFI, reading raw memory
| ```.as_mut_ptr()``` | ```*mut T``` | Mutable | Unsafe | FFI, modifying raw memory

#### Library updating:
Library creation implemented ```Public mod``` blocks and you can use them by: ```my_lib.Custom_Mod.some_function()``` example:
```ruby
    public mod Example:
        func red():
            return (255, 0, 0)
        end

        func green():
            return (0, 255, 0)
        end
    end
```
- Example usage:
```ruby
   import example_lib

    func main():
      print(example_lib.Example.red("Hello world"))
    end
```

#### > Update Alpha 3.0v: 1/10/2026.