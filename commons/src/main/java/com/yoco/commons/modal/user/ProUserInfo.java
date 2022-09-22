package com.yoco.commons.modal.user;

import lombok.Data;

@Data
public class ProUserInfo {
    private String accountID;
    private String contactID;
    private String emailID;
    private String role;
    private String empID;
    private String rfID;

    public ProUserInfo(String accountID, String contactID, String emailID, String role, String empID, String rfID){
        this.accountID = accountID;
        this.contactID = contactID;
        this.emailID = emailID;
        this.role = role;
        this.empID = empID;
        this.rfID = rfID;
    }
}
