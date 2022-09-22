package com.yoco.client.enums;

import lombok.NoArgsConstructor;

@NoArgsConstructor
public enum CLIENT_ERROR_RESPONSE {

        INVALID_EMAIL ( "Client email id is invalid" ),
        INVALID_NAME ( "Client name is invalid" ),
        CLIENT_ALREADY_EXIST ( "Client already exist"),
        NO_CLIENT_EXISTS("No client is available"),
        UNABLE_TO_CREATE_CLIENT("Unable to create client"),
        INVALID_CONTACT_ID("Invalid contactID"),
        INVALID_CLIENT_ID("Invalid Client ID");

        private String message;

        CLIENT_ERROR_RESPONSE(String message ) {
            this.message = message;
        }

        public String value ()
        {
            return message;
        }


}
