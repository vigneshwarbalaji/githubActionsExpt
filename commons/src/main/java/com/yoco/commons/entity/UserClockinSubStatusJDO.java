package com.yoco.commons.entity;

import com.googlecode.objectify.annotation.Entity;
import com.googlecode.objectify.annotation.Id;
import com.googlecode.objectify.annotation.Ignore;
import com.googlecode.objectify.annotation.Index;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.HashUtil;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.Date;

@Data
@NoArgsConstructor
@Entity
public class UserClockinSubStatusJDO implements Serializable {

    private static final long serialVersionUID = -5106842569995598653L;

    @Id
    String userClockInSubStatusID;

    @Index
    String uniquepin;

    @Index
    String contactId;

    @Index
    String projectId;

    String userLogIn;

    @Index
    String subStatus;

    @Index
    String statusType;

    String connectionId;

    @Index
    Date EventDate;

    @Index
    Long eventLongTime;

    Long receivedLongTime;

    @Ignore
    String eventDateText;

    @Ignore
    String eventTimeText;

    @Ignore
    String eventDateTime;

    public UserClockinSubStatusJDO(String accountID, String contactID, String projectID, String userLogin, String userSubStatus,
                                   String userStatusType, Long eventLongTime){

        this.userClockInSubStatusID = HashUtil.generateUUID();
        this.eventLongTime          = eventLongTime == 0L ? DateUtil.getCurrentTime(): eventLongTime;
        this.EventDate              = new Date(this.eventLongTime);
        this.uniquepin              = accountID;
        this.contactId              = contactID;
        this.projectId              = projectID;
        this.userLogIn              = userLogin;
        this.subStatus              = userSubStatus;
        this.statusType             = userStatusType;
        this.connectionId           = "";
        this.receivedLongTime       = eventLongTime;
    }

}
