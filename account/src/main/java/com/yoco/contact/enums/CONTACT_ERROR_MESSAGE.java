package com.yoco.contact.enums;

import lombok.NoArgsConstructor;

@NoArgsConstructor
public enum CONTACT_ERROR_MESSAGE {

    MAIL_DOES_NOT_EXIST("Mail does not exists"),
    INVALID_VERIFICATION_ID("verification id is not valid"),
    INVALID_PASSWORD("password is not valid"),
    CONTACT_DOES_NOT_EXIST("Contact doesn't exists"),
    INVALID_PASSWORD_LENGTH("Password must contain 8 to 64 characters"),
    INVALID_PASSWORD_TRAILING_SPACE("Password must not contain leading and trailing spaces"),
    INVALID_PASSWORD_COMBINATION("Password must contain atleast one number and one alphabet");

    private String message;

    CONTACT_ERROR_MESSAGE(String message ) {
        this.message = message;
    }

    public String value ()
    {
        return message;
    }

}
