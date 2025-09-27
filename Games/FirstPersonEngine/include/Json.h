#pragma once

#include <map>
#include <optional>
#include <string>
#include <vector>

class JsonValue {
  public:
    enum class Type {
        Null,
        Object,
        Array,
        String,
        Number,
        Boolean,
    };

    using Object = std::map<std::string, JsonValue>;
    using Array = std::vector<JsonValue>;

    JsonValue();
    static JsonValue makeNull();
    static JsonValue makeObject(Object value);
    static JsonValue makeArray(Array value);
    static JsonValue makeString(std::string value);
    static JsonValue makeNumber(double value);
    static JsonValue makeBoolean(bool value);

    Type type() const { return valueType; }
    bool isObject() const { return valueType == Type::Object; }
    bool isArray() const { return valueType == Type::Array; }
    bool isString() const { return valueType == Type::String; }
    bool isNumber() const { return valueType == Type::Number; }
    bool isBoolean() const { return valueType == Type::Boolean; }
    bool isNull() const { return valueType == Type::Null; }

    const Object &asObject() const;
    const Array &asArray() const;
    std::optional<std::string> asString() const;
    std::optional<double> asNumber() const;
    std::optional<bool> asBoolean() const;

    const JsonValue *find(const std::string &key) const;

    double numberOr(double fallback) const;

  private:
    Type valueType;
    Object objectValue;
    Array arrayValue;
    std::string stringValue;
    double numberValue;
    bool boolValue;
};

std::optional<JsonValue> parseJson(const std::string &text, std::string &errorMessage);

