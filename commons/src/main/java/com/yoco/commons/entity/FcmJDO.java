package com.yoco.commons.entity;

import com.googlecode.objectify.annotation.Entity;
import com.googlecode.objectify.annotation.Id;
import com.googlecode.objectify.annotation.Index;
import com.yoco.commons.utils.DateUtil;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.Date;

@Data
@NoArgsConstructor
@Entity
public class FcmJDO implements Serializable {

    @Id
    private String deviceID;

    @Index
    private String contactID;

    @Index
    private String deviceModel;

    @Index
    private String emailID;

    @Index
    private String accountID;

    @Index
    private String fcmDeviceToken;

    @Index
    private String role;

    @Index
    private String deviceType;

    @Index
    private String appVersion;

    @Index
    private String syncObject = "";

    @Index
    private Date dateCreated  = new Date(DateUtil.getCurrentTime());

}
