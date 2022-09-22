package com.yoco.user.enums;

import lombok.NoArgsConstructor;

@NoArgsConstructor
public enum USER_ERROR_MESSAGE {

    USER_NOT_FOUND ("user not found in the account"),
    NO_POLICY_EXISTS(" User doesn't have policy under this account "),
    ACCESS_VALUE_BOOLEAN_ERROR_MESSAGE("Access value must be a boolean."),
    NO_USERS_FOUND("No users found"),
    INVALID_ROLE("Invalid role"),
    EMAIL_ID_EXISTS("EmailID already exists");

    private String message;

    USER_ERROR_MESSAGE(String message ) {
        this.message = message;
    }

    public String value ()
    {
        return message;
    }
}