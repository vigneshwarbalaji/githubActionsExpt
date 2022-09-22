package com.yoco.zone.enums;

import lombok.NoArgsConstructor;

@NoArgsConstructor
public enum ZONE_ERROR_MESSAGE {

    INVALID_SYSTEM_OFFSET("Invalid system offset"),
    ERROR_ON_GETTING_DATES("Error on getting dates");

    private String message;

    ZONE_ERROR_MESSAGE(String message ) {
        this.message = message;
    }


    public String value ()
    {
        return message;
    }
}
