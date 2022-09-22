package com.yoco.commons.entity;

import com.googlecode.objectify.annotation.Entity;
import com.googlecode.objectify.annotation.Id;
import com.googlecode.objectify.annotation.Index;
import com.googlecode.objectify.annotation.Unindex;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.HashUtil;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@Entity
public class IntegrationsJDO {

    private static final long serialVersionUID = 2651839692102895020L;

    @Id
    private String integrationID;

    @Index
    private String integrationType;

    @Index
    private Boolean isActive;

    @Index
    private String uniquePin;

    @Index
    private String contactID;

    @Unindex
    private String integrationDetails;

    @Index
    private Long dateAddedLongTime;

    public IntegrationsJDO(String accountID, String contactID, String type, String integrationDetailsMap, boolean isActive){
        this.integrationID          =   HashUtil.generateUUID();
        this.uniquePin              =   accountID;
        this.contactID              =   contactID;
        this.integrationType        =   type;
        this.dateAddedLongTime      =   DateUtil.getCurrentTime();
        this.integrationDetails     =   integrationDetailsMap;
        this.isActive               =   isActive;
    }

}
