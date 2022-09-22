package com.yoco.enums;

import lombok.NoArgsConstructor;

@NoArgsConstructor
public enum ADJUSTMENTS_ERROR_RESPONSE {

    ACTION_ALREADY_TAKEN("Someone already took an action on this adjustment."),
    ADJUSTMENT_OVERLAP("Adjusted time overlaps with existing entry"),
    INVALID_IN_TIME("Invalid in time"),
    INVALID_OUT_TIME("Invalid out time"),
    INVALID_MESSAGE("Please enter an valid adjustment reason"),
    INVALID_DESCRIPTION("Invalid description"),
    IN_TIME_GREATER_THAN_OUT_TIME("Out time must be greater than in time"),
    ADJUSTED_TIME_GREATER_THAN_CURRENT_TIME("Adjusted time must be earlier than current time.");

    private String message;

    ADJUSTMENTS_ERROR_RESPONSE  ( String message )
    {
        this.message = message;
    }

    public String value ()
    {
        return message;
    }
}
