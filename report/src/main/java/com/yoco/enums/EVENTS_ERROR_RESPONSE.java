package com.yoco.enums;

import lombok.NoArgsConstructor;

@NoArgsConstructor
public enum EVENTS_ERROR_RESPONSE {

    NO_ENTRIES_FOUND("No entries available"),
    NO_SKILL_SET ("No proper skill set"),
    NOT_AUTHORIZED ("User not authorized"),
    INVALID_IP_ADDRESS(" Invalid IP address"),
    NO_ACCOUNT_FOUND ("No account found"),
    OPERATION_FAILED("Operation failed"),
    NO_CLOCKEDIN_USERS("No ClockedInUsers"),
    UNABLE_TO_DELETE("Unable to delete entry");
    private String message;

    EVENTS_ERROR_RESPONSE  ( String message )
    {
        this.message = message;
    }

    public String value ()
    {
        return message;
    }
}
