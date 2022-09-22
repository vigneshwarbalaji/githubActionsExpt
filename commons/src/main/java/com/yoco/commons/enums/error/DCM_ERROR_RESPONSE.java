package com.yoco.commons.enums.error;

import lombok.NoArgsConstructor;

@NoArgsConstructor
public enum DCM_ERROR_RESPONSE {

    NOT_ALLOWED_IN_DCM ("Not allowed in DCM" ),
    FAILED_TO_FETCH_FROM_DCM ("Failed to fetch from dcm" );

    private String message;

    DCM_ERROR_RESPONSE(String message ) {
        this.message = message;
    }

    public String value ()
    {
        return message;
    }
}
