package com.yoco.commons.enums.error;

import com.fasterxml.jackson.annotation.JsonValue;

import java.io.Serializable;

public interface ErrorCode extends Serializable {

    @JsonValue
    String getErrorCode();

    String toString();
}
