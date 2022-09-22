package com.yoco.fcm.enums;

import lombok.NoArgsConstructor;

@NoArgsConstructor
public enum FCM_ERROR_MESSAGE {

    USER_NOT_ASSOCIATED ("User not associated"),
    INVALID_DEVICE_ID ("DeviceID is null"),
    DEVICE_ID_NOT_FOUND ("DeviceID not found"),
    INVALID_PAYLOAD("Invalid Payload"),
    EMPTY_PAYLOAD("Payload is empty"),
    DEVICE_NOT_FOUND("Device not found");

    private String message;

    FCM_ERROR_MESSAGE(String message ) {
        this.message = message;
    }

    public String value ()
    {
        return message;
    }
}
