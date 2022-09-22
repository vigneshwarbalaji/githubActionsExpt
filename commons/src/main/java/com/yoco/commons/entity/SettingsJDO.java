package com.yoco.commons.entity;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.google.appengine.api.datastore.Text;
import com.googlecode.objectify.annotation.Entity;
import com.googlecode.objectify.annotation.Id;
import com.googlecode.objectify.annotation.Index;
import com.googlecode.objectify.annotation.Unindex;
import com.yoco.commons.constants.ClientSource;
import com.yoco.commons.constants.DateConstants;
import com.yoco.commons.enums.Status;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.commons.utils.TimeZoneUtil;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.Date;
import java.util.Locale;
import java.util.Map;

@Data
@NoArgsConstructor
@Entity
public class SettingsJDO implements Serializable {

    private static final long serialVersionUID = 3667136980203902399L;

    @Id
    @JsonProperty("id")
    private String peopleUniquePin;

    @Index
    @JsonProperty("sourceEmail")
    private String sourceEmail;

    private String domainName;

    @Index
    private Long dateAddedLongTime;

    @Unindex
    private String signUpSource;

    private String timeZone;

    @JsonProperty("accountName")
    private String displayDomainName;

    private Date dateAdded;

    @JsonProperty("timeZoneInfo")
    private String timeZoneDisplayName;

    private Boolean isForceClockedOut; //not used anywhere

    private Boolean isContactManipulated; //not used anywhere

    private Boolean isExcessClockedInAlert;

    @Unindex
    private String allowedIPAddresses;

    @JsonProperty("otThresholdHours")
    private String otThresholdHours;

    @JsonProperty("weekStartDay")
    private String weekStartDay;

    @JsonProperty("isPayrollEnabled")
    private Boolean isPayrollEnabled;

    @JsonProperty("payrollAccessSince")
    private Long payrollAccessSince;

    @JsonProperty("source")
    private String source;

    @JsonProperty("displayTimeFormat")
    private String displayTimeFormat;

    @JsonProperty("isNfcRfidEnabled")
    private Boolean isNfcRfidEnabled;

    @JsonProperty("registrationDetails")
    @Unindex
    private String registeredPersonDetails;

    private String status;

    private Long dateDeletedLongTime;

    public SettingsJDO (Map<String,Object> account){

        this.setPeopleUniquePin(account.get("accountID").toString());
        this.setSourceEmail(account.get("sourceEmail").toString());
        this.setDomainName(account.get("accountName").toString());
        this.setDisplayDomainName(account.get("accountName").toString());
        this.setTimeZone(account.get("timeZone").toString());
        this.setTimeZoneDisplayName(account.get("timeZoneDisplayName").toString());
        this.setSource(ClientSource.getClientIdMap().get(account.get("srcClientID").toString()));
        this.setRegisteredPersonDetails(JsonUtil.getJson(account.get("registrantInfo")));
        this.setSignUpSource(account.get("signUpSource").toString());
        this.setDateAddedLongTime(DateUtil.getCurrentTime());
        this.setDateAdded(new Date(this.dateAddedLongTime));
        this.setStatus(Status.ACTIVE.toString());

        this.assignDefaultValues();
    }

    public SettingsJDO(String id, String email, String accountName, String timezone, String source, Map<String,Object> registrantInfo, String srcRef){
        this.setPeopleUniquePin(id);
        this.setSourceEmail(email);
        this.setDomainName(accountName);
        this.setDisplayDomainName(accountName);
        this.setTimeZone(timezone);
        this.setTimeZoneDisplayName(TimeZoneUtil.getZoneInFormat(timezone,TimeZoneUtil.OFFSET_ZONE_DISPLAY_NAME_LONG, Locale.ENGLISH));
        this.setSource(source);
        this.setRegisteredPersonDetails(JsonUtil.getJson(registrantInfo));
        this.setSignUpSource(srcRef);
        this.setStatus(Status.ACTIVE.toString());
        this.assignDefaultValues();
    }

    private void assignDefaultValues(){
        this.setIsForceClockedOut(true);
        this.setIsContactManipulated(true);
        this.setIsPayrollEnabled(true);
        this.setIsExcessClockedInAlert(false);
        this.setIsNfcRfidEnabled(false);
        this.setAllowedIPAddresses("");
        this.setOtThresholdHours("");
        this.setWeekStartDay(DateConstants.WEEK_DAY_SUN);
        this.setDisplayTimeFormat(DateConstants.DEFAULT_TIME_FORMAT);
        this.setPayrollAccessSince(0l);
        this.setDateDeletedLongTime(0l);
    }

}
