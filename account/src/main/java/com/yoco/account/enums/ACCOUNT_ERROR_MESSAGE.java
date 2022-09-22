package com.yoco.account.enums;

import lombok.NoArgsConstructor;

@NoArgsConstructor
public enum ACCOUNT_ERROR_MESSAGE {

    MAX_USERS_COUNT_LIMIT ("Upgrade your plan to add more staff"),
    INVALID_EMAIL("EmailID is invalid"),
    INVALID_USERNAME("Username is invalid"),
    INTERNAL_DOMAIN("Can not sign up with internal domains."),
    ACCOUNT_EXISTS("Email is already registered. Please login");

    private String message;

    ACCOUNT_ERROR_MESSAGE(String message ) {
        this.message = message;
    }

    public String value ()
    {
        return message;
    }

}