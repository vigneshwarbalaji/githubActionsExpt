package com.yoco.commons.entity;

import com.googlecode.objectify.annotation.*;
import com.yoco.commons.modal.user.DataProtectionSkillset;
import com.yoco.commons.modal.user.ProUserInfo;
import com.yoco.commons.modal.user.Skillset;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.HashUtil;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.io.Serializable;
import java.util.Date;

@Data
@NoArgsConstructor
@Entity
public class PeopleRelationJDO implements Serializable {

    private static final long serialVersionUID = 7754967951629628584L;

    @Id
    private String id;

    @Index
    private String contactId;

    @Index
    private String uniquepin;

    @Index
    private String emailID ;

    @Index
    private String parentContactId;

    @Index
    private String role;

    @Index
    private boolean isDelete;

    @Index
    private boolean isDefault;

    private Date dateAdded;

    @Index
    private Long dateAddedLongTime;

    @Index
    private Long dateModified;

    @Index
    private String login;

    @Index
    private String timeZone;

    private String timeZoneDisplayName;

    @Unindex
    String skillsets;

    private boolean toBeMonitoredForExcessClockedIn;

    private Long canAccessReportsSince;

    private Long hireDate;

    private Long startDate;

    private Long relieveDate;

    @Index
    private String rfId;

    @Unindex
    String dataProtectionSkillset;

    @Ignore
    private Contact contact;

    public PeopleRelationJDO (ProUserInfo userInfo, String parent,
                              String timezone, String timeZoneDisplayName, Boolean isDefault, boolean isCompanyPayrollEnabled ){
        this.id                                 =       HashUtil.generateUUID();
        this.isDelete                           =       false;
        this.dateAddedLongTime                  =       DateUtil.getCurrentTime();
        this.dateAdded                          =       new Date(this.dateAddedLongTime);
        this.dateModified                       =       this.dateAddedLongTime;
        this.toBeMonitoredForExcessClockedIn    =       false;
        this.canAccessReportsSince              =       0L;
        this.hireDate                           =       0L;
        this.startDate                          =       0L;
        this.relieveDate                        =       0L;
        this.parentContactId                    =       parent;
        this.contactId                          =       userInfo.getContactID();
        this.uniquepin                          =       userInfo.getAccountID();
        this.emailID                            =       userInfo.getEmailID();
        this.role                               =       userInfo.getRole();
        this.login                              =       userInfo.getEmpID();
        this.timeZone                           =       timezone;
        this.timeZoneDisplayName                =       timeZoneDisplayName;
        this.skillsets                          =       String.valueOf(Skillset.createSkillsetText( role , userInfo.getAccountID(), isCompanyPayrollEnabled).getValue());
        this.dataProtectionSkillset             =       String.valueOf(DataProtectionSkillset.getDefaultUserDataProtectionSkillset().getValue());
        this.rfId                               =       userInfo.getRfID();
        this.isDefault                          =       isDefault;
    }

}
