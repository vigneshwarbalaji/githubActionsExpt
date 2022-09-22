package com.yoco.activity.enums;

public enum ACTIVITY_ERROR_RESPONSE {

    ACTIVITIES_NOT_FOUND(" No Activities are there"),
    EVENT_TIME_GREATER_THAN_CURRENT_TIME("Event happened time cannot be greater than current time."),
    INVALID_START_TIME("Invalid Start Time");

    private String message;

    ACTIVITY_ERROR_RESPONSE(String message ) {
        this.message = message;
    }


    public String value ()
    {
        return message;
    }
}
