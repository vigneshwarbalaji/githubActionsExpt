package com.yoco.project.enums;

import lombok.NoArgsConstructor;

@NoArgsConstructor
public enum PROJECT_ERROR_RESPONSE {

    INVALID_PAYLOAD_PROPERTY ( "Payload does not contain name as property. "),
    PROJECT_NOT_FOUND ( "no project found"),
    PROJECT_NOT_ASSOCIATED("project not associated"),
    INVALID_PROJECT_ID ( "Invalid projectID." ),
    INVALID_PROJECT_NAME ( "Project name already exists or invalid");

    private String message;

    PROJECT_ERROR_RESPONSE  ( String message )
    {
        this.message = message;
    }

    public String value ()
    {
        return message;
    }
}
