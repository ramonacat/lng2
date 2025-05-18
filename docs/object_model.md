# The object model
Every object is represented in memory by a structure.
The structure contains:
    - The type (which is also an object)
    - Fields

Each field has a type, which can be either primitive (u8, u16, u32, u64, i8, ..., f32, f64), a type-erased object (the type is an interface) or an exact object.
The type is an object which contains:
    - a unique identifier
    - Instance field descriptors (interned name, value type)
    - Static field descriptors (interned name, value type, the value itself)
    - A list of implemented interfaces

It is important to note, that functions are not listed here. A function is a type of an object that represents the Callable built-in interface.
A type descriptor can implement the Generic built-in interface, in which case, before an object of that type can be created, the type needs to be instantiated.

## Built-in types
### Object
This is the only type that does not have a type itself, all the other objects have it or it's descendants as their type.

### Interface (implements TypeDescriptor, instance of Object)

TODO should TypeDescriptor be separate from a TypeConstraint (with the latter being used to explain a "matched" type, e.g. the allowed types of fields)?

The type of all interfaces.
Contains a single field which is a HashMap, where the key is the name of the field, and the value is the TypeDescriptor (commonly an instantiated Callable).

### Generic 
Signifies that a TypeDescriptor must be instantiated with type arguments before it can be used as on object type.

### TypeDescriptor (instance of Interface)
All the type descriptors are instances of it.

### Callable 
This type defines a method `call` for calling the callable. It is variadically generic over arguments, and return type. The objects are implemented with compiler magic.

### Vector
A Generic object that stores objects of the type in its type parameter in a contiguous block of memory. 
