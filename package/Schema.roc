module [
    Schema,
    SchemaInputObject,
    SchemaOutputObject,
    SchemaObjectField,
    SchemaFieldParameter,
    CustomScalar,
    SchemaEnum,
    NullableDataType,
    DataType,
    SchemaUnion,
]

# TODO: "directive" and "extend schema"

Schema : {
    customScalars : List CustomScalar,
    enums : List SchemaEnum,
    unions : List SchemaUnion,
    inputObjects : List SchemaInputObject,
    outputObjects : List SchemaOutputObject,
    queryRoot : SchemaOutputObject,
    mutationRoot : SchemaOutputObject,
    subscriptionRoot : SchemaOutputObject,
}

SchemaInputObject : {
    name : Str,
    annotations : List Str,
    fields : List SchemaFieldParameter,
}

SchemaOutputObject : {
    name : Str,
    annotations : List Str,
    fields : List SchemaObjectField,
}

SchemaObjectField : {
    name : Str,
    description : Str,
    parameters : List SchemaFieldParameter,
    type : NullableDataType,
}

SchemaFieldParameter : {
    name : Str,
    description : Str,
    type : NullableDataType,
    default : Result Str [NoDefault],
}

CustomScalar : {
    name : Str,
    description : Str,
}

SchemaEnum : {
    name : Str,
    variants : List Str,
}

NullableDataType : [
    Nullable DataType,
    NotNull DataType,
]

DataType : [
    ID,
    String,
    Int,
    Float,
    Boolean,
    List NullableDataType,
    # Needs to be a reference to avoid circular definitions
    Object Str,
    CustomScalar Str,
]

SchemaUnion : {
    name : Str,
    members : List Str,
}
