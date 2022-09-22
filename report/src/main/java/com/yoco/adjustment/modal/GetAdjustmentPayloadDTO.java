package com.yoco.adjustment.modal;

import com.yoco.commons.entity.Contact;
import lombok.Data;

import java.io.Serializable;

@Data
public class GetAdjustmentPayloadDTO implements Serializable {
    private String contactID;
    private String range;
    private String from;
    private String to;
    private String timeZone;
    private Contact loggedInUserContact;

    public GetAdjustmentPayloadDTO(String contactID, String range, String from, String to, String timeZone, Contact loggedInUserContact) {
        this.contactID = contactID;
        this.range = range;
        this.from = from;
        this.to = to;
        this.timeZone = timeZone;
        this.loggedInUserContact = loggedInUserContact;
    }
}
