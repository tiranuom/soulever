Soulever TODOS
==============

- Remove TypeFieldProviders and KindFieldProviders, replace with FieldProviders. 
    - TypeFieldProviders as `implicit val`s 
    - KindFieldProviders as `implicit def`s.
- Fields as functions : 
    - apply : `(Req) => Either[Resp, A]`
    - setValue : `(Req) => Resp`
- Attribute annotations :
    - css
    - mapping
    - section
- Single import object and annotations in traits


Design Goals
============
- Userfriendlyness 
    - In usage,
        - Single import object 