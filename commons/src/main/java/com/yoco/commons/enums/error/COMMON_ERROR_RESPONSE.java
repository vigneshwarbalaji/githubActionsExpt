package com.yoco.commons.enums.error;

import lombok.NoArgsConstructor;

@NoArgsConstructor
public enum COMMON_ERROR_RESPONSE {

    INVALID_ACCOUNT_ID ("Invalid accountID." ),
    INVALID_CONTACT_ID ("Invalid contactID."),
    INVALID_EMAIL_ID ( "Invalid emailID"),
    INVALID_ENTRY_ID ( "Invalid entryID"),
    UNABLE_TO_CREATE("Unable to create "),
    INVALID_PAYLOAD ("Payload cannot be empty or null." ),
    USER_NOT_AUTHORIZED ("User not authorized"),
    FROM_AND_TO_DATE_MANDATORY("From and To dates required on By-Date range"),
    INVALID_RANGE ("Invalid range"),
    INVALID_STATUS("Invalid_status"),
    ACCOUNT_NOT_FOUND("account not found"),
    OPERATION_FAILED("Operation failed"),
    FAILED_TO_UPDATE_THE_EVENT("Deleted event cannot be updated"),
    ACTION_ALREADY_TAKEN("Someone already took an action on this adjustment."),
    INVALID_DATEFORMAT("Invalid date string format"),
    FAILED_TO_PERSIST("Error on saving entity"),
    USER_NOT_FOUND ("User not found"),
    INVALID_CONSTRAINTS ( "No proper constraints. " ),
    INVALID_PAYLOAD_CONTAINS_ID ( "Malformed object. Payload contains the attribute 'id'."),
    REQUEST_LIMIT_REACHED ("Too many attempts, try again later");

    private String message;

    COMMON_ERROR_RESPONSE(String message ) {
        this.message = message;
    }

    public String value ()
    {
        return message;
    }

}
