# Soulever


Soulever is a simple macro based approach to generate UI forms out of case classes.

Currently supports vaadin 7.0 backend.

# Goals

Simplicity, Extensibility, Flexibility, Consistency.

Creating complex and validation rich forms should be really simple.

# Features

#### 1. Support for type based field generation

- Basic types like, _String_ and `Int` to relevant field implementations with validations.
- Type constructors like `Option` or `List` to relevant field implementations, which wraps the correct inner type( *type parameter* ).
- Custom types where `Type Provider` is present or custom type constructors where `Kind Provider` is present.

#### 2. Support for the annotated field validations.

- Basic validations like `min`, `max` and `nonEmpty`
- Custom validations where validation is passed as a function

#### 3. Automated i18n key generation for fields, buttons and validation errors.

- Class name will be mapped to dot separated lower case keys.
- Field names will be mapped to class i18n key with dot separated lower case keys prefixed separated by a dot(\.).
- Validation errors will be mapped to field i18n key with square bracket enclosed error key.
- Button headers will be mapped to class i18n key with class i18n key with dot separated given button key.

# Whats not supported.

- Kind field providers for types which has type parameter arity greater than 1 ( *ex: `Either[R,L]`, `Map[K,V]`* )
- Combo boxes are not supported yet
- Complex forms with multiple columns.
- Sectioned forms are yet to be implemented.

# Get soulever

- Still not published a stable release.

# Documentation
*TODO*

