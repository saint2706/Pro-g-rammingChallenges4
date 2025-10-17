#include "Json.h"

#include <cctype>
#include <charconv>
#include <system_error>
#include <stdexcept>
#include <string_view>

namespace {
class Parser {
  public:
    explicit Parser(std::string_view text) : input(text) {}

    std::optional<JsonValue> parse(std::string &error) {
        skipWhitespace();
        auto value = parseValue(error);
        if (!value) {
            return std::nullopt;
        }
        skipWhitespace();
        if (position != input.size()) {
            error = "Unexpected trailing characters in JSON input";
            return std::nullopt;
        }
        return value;
    }

  private:
    std::string_view input;
    size_t position{0};

    void skipWhitespace() {
        while (position < input.size()) {
            unsigned char c = static_cast<unsigned char>(input[position]);
            if (c == ' ' || c == '\n' || c == '\r' || c == '\t') {
                ++position;
            } else {
                break;
            }
        }
    }

    bool consume(char expected) {
        if (position < input.size() && input[position] == expected) {
            ++position;
            return true;
        }
        return false;
    }

    std::optional<JsonValue> parseValue(std::string &error) {
        skipWhitespace();
        if (position >= input.size()) {
            error = "Unexpected end of JSON input";
            return std::nullopt;
        }
        char c = input[position];
        if (c == '"') {
            auto stringValue = parseString(error);
            if (!stringValue) {
                return std::nullopt;
            }
            return JsonValue::makeString(*stringValue);
        }
        if (c == '{') {
            return parseObject(error);
        }
        if (c == '[') {
            return parseArray(error);
        }
        if (c == 't' || c == 'f') {
            return parseBoolean(error);
        }
        if (c == 'n') {
            return parseNull(error);
        }
        if (c == '-' || std::isdigit(static_cast<unsigned char>(c))) {
            return parseNumber(error);
        }
        error = "Unexpected token in JSON input";
        return std::nullopt;
    }

    std::optional<JsonValue> parseObject(std::string &error) {
        if (!consume('{')) {
            error = "Expected '{' at start of JSON object";
            return std::nullopt;
        }
        JsonValue::Object object;
        skipWhitespace();
        if (consume('}')) {
            return JsonValue::makeObject(std::move(object));
        }
        while (true) {
            skipWhitespace();
            auto key = parseString(error);
            if (!key) {
                if (error.empty()) {
                    error = "Expected string key in JSON object";
                }
                return std::nullopt;
            }
            skipWhitespace();
            if (!consume(':')) {
                error = "Expected ':' after key in JSON object";
                return std::nullopt;
            }
            skipWhitespace();
            auto value = parseValue(error);
            if (!value) {
                return std::nullopt;
            }
            object.emplace(std::move(*key), std::move(*value));
            skipWhitespace();
            if (consume('}')) {
                break;
            }
            if (!consume(',')) {
                error = "Expected ',' or '}' in JSON object";
                return std::nullopt;
            }
        }
        return JsonValue::makeObject(std::move(object));
    }

    std::optional<JsonValue> parseArray(std::string &error) {
        if (!consume('[')) {
            error = "Expected '[' at start of JSON array";
            return std::nullopt;
        }
        JsonValue::Array array;
        skipWhitespace();
        if (consume(']')) {
            return JsonValue::makeArray(std::move(array));
        }
        while (true) {
            auto value = parseValue(error);
            if (!value) {
                return std::nullopt;
            }
            array.emplace_back(std::move(*value));
            skipWhitespace();
            if (consume(']')) {
                break;
            }
            if (!consume(',')) {
                error = "Expected ',' or ']' in JSON array";
                return std::nullopt;
            }
        }
        return JsonValue::makeArray(std::move(array));
    }

    static void appendCodepoint(std::string &out, unsigned int codepoint) {
        if (codepoint <= 0x7F) {
            out.push_back(static_cast<char>(codepoint));
        } else if (codepoint <= 0x7FF) {
            out.push_back(static_cast<char>(0xC0 | (codepoint >> 6)));
            out.push_back(static_cast<char>(0x80 | (codepoint & 0x3F)));
        } else if (codepoint <= 0xFFFF) {
            out.push_back(static_cast<char>(0xE0 | (codepoint >> 12)));
            out.push_back(static_cast<char>(0x80 | ((codepoint >> 6) & 0x3F)));
            out.push_back(static_cast<char>(0x80 | (codepoint & 0x3F)));
        } else {
            out.push_back(static_cast<char>(0xF0 | (codepoint >> 18)));
            out.push_back(static_cast<char>(0x80 | ((codepoint >> 12) & 0x3F)));
            out.push_back(static_cast<char>(0x80 | ((codepoint >> 6) & 0x3F)));
            out.push_back(static_cast<char>(0x80 | (codepoint & 0x3F)));
        }
    }

    std::optional<std::string> parseString(std::string &error) {
        if (!consume('"')) {
            error = "Expected '"' at start of JSON string";
            return std::nullopt;
        }
        std::string result;
        while (position < input.size()) {
            char c = input[position++];
            if (c == '"') {
                return result;
            }
            if (static_cast<unsigned char>(c) < 0x20) {
                error = "Invalid control character in JSON string";
                return std::nullopt;
            }
            if (c == '\\') {
                if (position >= input.size()) {
                    error = "Invalid escape sequence in JSON string";
                    return std::nullopt;
                }
                char esc = input[position++];
                switch (esc) {
                case '"':
                    result.push_back('"');
                    break;
                case '\\':
                    result.push_back('\\');
                    break;
                case '/':
                    result.push_back('/');
                    break;
                case 'b':
                    result.push_back('\b');
                    break;
                case 'f':
                    result.push_back('\f');
                    break;
                case 'n':
                    result.push_back('\n');
                    break;
                case 'r':
                    result.push_back('\r');
                    break;
                case 't':
                    result.push_back('\t');
                    break;
                case 'u': {
                    auto codepoint = parseUnicodeEscape(error);
                    if (!codepoint) {
                        return std::nullopt;
                    }
                    appendCodepoint(result, *codepoint);
                    break;
                }
                default:
                    error = "Unknown escape sequence in JSON string";
                    return std::nullopt;
                }
            } else {
                result.push_back(c);
            }
        }
        error = "Unterminated JSON string";
        return std::nullopt;
    }

    std::optional<unsigned int> parseUnicodeEscape(std::string &error) {
        if (position + 4 > input.size()) {
            error = "Incomplete unicode escape in JSON string";
            return std::nullopt;
        }
        unsigned int value = 0;
        for (int i = 0; i < 4; ++i) {
            char c = input[position++];
            unsigned int digit = 0;
            if (c >= '0' && c <= '9') {
                digit = static_cast<unsigned int>(c - '0');
            } else if (c >= 'a' && c <= 'f') {
                digit = static_cast<unsigned int>(c - 'a' + 10);
            } else if (c >= 'A' && c <= 'F') {
                digit = static_cast<unsigned int>(c - 'A' + 10);
            } else {
                error = "Invalid hex digit in unicode escape";
                return std::nullopt;
            }
            value = (value << 4) | digit;
        }
        if (value >= 0xD800 && value <= 0xDBFF) {
            // high surrogate, expect a following \uXXXX sequence
            if (!(position + 2 <= input.size() && input[position] == '\\' && input[position + 1] == 'u')) {
                error = "Invalid unicode surrogate pair";
                return std::nullopt;
            }
            position += 2; // consume \u
            auto low = parseUnicodeEscape(error);
            if (!low) {
                return std::nullopt;
            }
            if (*low < 0xDC00 || *low > 0xDFFF) {
                error = "Invalid low surrogate in unicode escape";
                return std::nullopt;
            }
            unsigned int highSurrogate = value - 0xD800;
            unsigned int lowSurrogate = *low - 0xDC00;
            return 0x10000 + ((highSurrogate << 10) | lowSurrogate);
        }
        if (value >= 0xDC00 && value <= 0xDFFF) {
            error = "Unexpected low surrogate without preceding high surrogate";
            return std::nullopt;
        }
        return value;
    }

    std::optional<JsonValue> parseNumber(std::string &error) {
        size_t start = position;
        if (consume('-')) {
            if (position >= input.size()) {
                error = "Invalid JSON number";
                return std::nullopt;
            }
        }
        if (consume('0')) {
            // leading zero is allowed only when alone
        } else {
            if (position >= input.size() || !std::isdigit(static_cast<unsigned char>(input[position]))) {
                error = "Invalid JSON number";
                return std::nullopt;
            }
            while (position < input.size() && std::isdigit(static_cast<unsigned char>(input[position]))) {
                ++position;
            }
        }
        if (consume('.')) {
            if (position >= input.size() || !std::isdigit(static_cast<unsigned char>(input[position]))) {
                error = "Invalid JSON number";
                return std::nullopt;
            }
            while (position < input.size() && std::isdigit(static_cast<unsigned char>(input[position]))) {
                ++position;
            }
        }
        if (position < input.size() && (input[position] == 'e' || input[position] == 'E')) {
            ++position;
            if (position < input.size() && (input[position] == '+' || input[position] == '-')) {
                ++position;
            }
            if (position >= input.size() || !std::isdigit(static_cast<unsigned char>(input[position]))) {
                error = "Invalid JSON exponent";
                return std::nullopt;
            }
            while (position < input.size() && std::isdigit(static_cast<unsigned char>(input[position]))) {
                ++position;
            }
        }
        std::string_view numberView = input.substr(start, position - start);
        double value = 0.0;
        auto result = std::from_chars(numberView.data(), numberView.data() + numberView.size(), value);
        if (result.ec == std::errc::invalid_argument || result.ec == std::errc::result_out_of_range) {
            try {
                value = std::stod(std::string(numberView));
            } catch (const std::exception &) {
                error = "Failed to convert JSON number";
                return std::nullopt;
            }
        }
        return JsonValue::makeNumber(value);
    }

    std::optional<JsonValue> parseBoolean(std::string &error) {
        if (input.substr(position, 4) == "true") {
            position += 4;
            return JsonValue::makeBoolean(true);
        }
        if (input.substr(position, 5) == "false") {
            position += 5;
            return JsonValue::makeBoolean(false);
        }
        error = "Invalid boolean literal in JSON";
        return std::nullopt;
    }

    std::optional<JsonValue> parseNull(std::string &error) {
        if (input.substr(position, 4) == "null") {
            position += 4;
            return JsonValue::makeNull();
        }
        error = "Invalid null literal in JSON";
        return std::nullopt;
    }
};
} // namespace

JsonValue::JsonValue() : valueType(Type::Null), numberValue(0.0), boolValue(false) {}

JsonValue JsonValue::makeNull() {
    return JsonValue();
}

JsonValue JsonValue::makeObject(Object value) {
    JsonValue result;
    result.valueType = Type::Object;
    result.objectValue = std::move(value);
    return result;
}

JsonValue JsonValue::makeArray(Array value) {
    JsonValue result;
    result.valueType = Type::Array;
    result.arrayValue = std::move(value);
    return result;
}

JsonValue JsonValue::makeString(std::string value) {
    JsonValue result;
    result.valueType = Type::String;
    result.stringValue = std::move(value);
    return result;
}

JsonValue JsonValue::makeNumber(double value) {
    JsonValue result;
    result.valueType = Type::Number;
    result.numberValue = value;
    return result;
}

JsonValue JsonValue::makeBoolean(bool value) {
    JsonValue result;
    result.valueType = Type::Boolean;
    result.boolValue = value;
    return result;
}

const JsonValue::Object &JsonValue::asObject() const {
    if (valueType != Type::Object) {
        throw std::logic_error("JSON value is not an object");
    }
    return objectValue;
}

const JsonValue::Array &JsonValue::asArray() const {
    if (valueType != Type::Array) {
        throw std::logic_error("JSON value is not an array");
    }
    return arrayValue;
}

std::optional<std::string> JsonValue::asString() const {
    if (valueType == Type::String) {
        return stringValue;
    }
    return std::nullopt;
}

std::optional<double> JsonValue::asNumber() const {
    if (valueType == Type::Number) {
        return numberValue;
    }
    return std::nullopt;
}

std::optional<bool> JsonValue::asBoolean() const {
    if (valueType == Type::Boolean) {
        return boolValue;
    }
    return std::nullopt;
}

const JsonValue *JsonValue::find(const std::string &key) const {
    if (valueType != Type::Object) {
        return nullptr;
    }
    auto it = objectValue.find(key);
    if (it == objectValue.end()) {
        return nullptr;
    }
    return &it->second;
}

double JsonValue::numberOr(double fallback) const {
    if (valueType == Type::Number) {
        return numberValue;
    }
    return fallback;
}

std::optional<JsonValue> parseJson(const std::string &text, std::string &errorMessage) {
    Parser parser(text);
    return parser.parse(errorMessage);
}

