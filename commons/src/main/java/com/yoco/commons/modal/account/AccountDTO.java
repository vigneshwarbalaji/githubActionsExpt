package com.yoco.commons.modal.account;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.yoco.commons.entity.SettingsJDO;
import com.yoco.commons.enums.Status;
import com.yoco.commons.utils.AccountUtil;
import lombok.Data;
import java.io.Serializable;
import java.util.HashMap;

@Data
public class AccountDTO implements Serializable {
    private String accountName = "";
    private String allowedIPAddresses = "";
    private String companyLogo = "";
    private String country = "";
    private String countryCode = "";
    private String displayTimeFormat = "";
    private long dateDeletedTime = 0L;
    private String id = "";
    private boolean isActive = true;
    @JsonProperty(value="isNfcRfidEnabled")
    private boolean isNfcRfidEnabled = false;
    @JsonProperty(value="isPayrollEnabled")
    private boolean isPayrollEnabled = false;
    private String nationalNumber = "";
    private String otThresholdHours = "";
    private long payrollAccessSince = 0L;
    private String payrollAccessSinceDate = "";
    private HashMap<String,Object> registrationDetails;
    private String sourceEmail = "";
    private String timeZoneInfo = "";
    private String weekStartDay = "";

    public AccountDTO(){}

    public AccountDTO(SettingsJDO account){
        this.accountName = account.getDisplayDomainName();
        this.allowedIPAddresses = account.getAllowedIPAddresses();
        this.registrationDetails = new HashMap<>(AccountUtil.getRegisteredAccountDetails(account));
        this.companyLogo = AccountUtil.getCompanyLogo(this.registrationDetails);
        this.displayTimeFormat = account.getDisplayTimeFormat();
        this.id = account.getPeopleUniquePin();
        if(account.getIsNfcRfidEnabled() != null){
            this.isNfcRfidEnabled = account.getIsNfcRfidEnabled();
        }
        this.isPayrollEnabled = account.getIsPayrollEnabled();
        this.payrollAccessSince = account.getPayrollAccessSince();
        this.payrollAccessSinceDate = AccountUtil.getPayrollAccessSinceDate(account);
        this.sourceEmail = account.getSourceEmail();
        this.timeZoneInfo = account.getTimeZoneDisplayName();
        this.weekStartDay = account.getWeekStartDay();
        this.otThresholdHours = account.getOtThresholdHours();
        var phoneNumberDetailsDTO = new PhoneNumberDetailsDTO(this.registrationDetails);
        this.country = phoneNumberDetailsDTO.getCountry();
        this.countryCode = phoneNumberDetailsDTO.getCountryPhoneCode();
        this.nationalNumber = phoneNumberDetailsDTO.getNationalNumber();
        this.dateDeletedTime = account.getDateDeletedLongTime();
        this.isActive = Status.ACTIVE.toString().equals(account.getStatus());
    }

}
