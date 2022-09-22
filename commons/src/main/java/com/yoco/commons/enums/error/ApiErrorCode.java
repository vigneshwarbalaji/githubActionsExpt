package com.yoco.commons.enums.error;

public enum ApiErrorCode implements ErrorCode {

    UNAUTHORIZED_REQUEST, NOT_FOUND, BAD_REQUEST, FORBIDDEN_REQUEST, INTERNAL_SERVER_ERROR, METHOD_NOT_ALLOWED,
    LIMITED_REACHED, INVALID_REQUEST, NO_CONTENT;

    @Override
    public String getErrorCode() {
        return this.name();
    }

    @Override
    public String toString() {
        return getErrorCode();
    }
}
