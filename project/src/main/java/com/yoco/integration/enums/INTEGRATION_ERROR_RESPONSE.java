package com.yoco.integration.enums;

import lombok.NoArgsConstructor;

@NoArgsConstructor
public enum INTEGRATION_ERROR_RESPONSE {

    UNABLE_TO_UPDATE_INTEGRATION("Unable to update integration details"),
    ACCESS_DENIED("Access denied"),
    INVALID_INTEGRATION_TYPE("Invalid integration type"),
    INVALID_TASKID("Invalid taskID");

    private String message;

    INTEGRATION_ERROR_RESPONSE(String message ) {
        this.message = message;
    }

    public String value ()
    {
        return message;
    }

}