package com.yoco.commons.modal.account;

import com.yoco.commons.utils.AccountUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.utils.account.PhoneNumberUtil;
import lombok.Data;
import java.io.Serializable;
import java.util.Map;

@Data
public class PhoneNumberDetailsDTO implements Serializable {
    private String country = "";
    private String countryPhoneCode = "";
    private String nationalNumber = "";

    public PhoneNumberDetailsDTO(){}

    public PhoneNumberDetailsDTO(Map<String,Object> registrationDetails){
        this(AccountUtil.getRegisteredCountryAlphaCode(registrationDetails),AccountUtil.getRegisteredPhoneNumber(registrationDetails));
    }

    public PhoneNumberDetailsDTO(String country, String phoneNumber){
        this.country = country;
        if(!ObjUtils.isNullOrEmpty(phoneNumber)){
            var userPhoneNumber = PhoneNumberUtil.extractUserPhoneNumber(phoneNumber,this.country);
            if(PhoneNumberUtil.isValidPhoneNumber(userPhoneNumber)){
                this.country = PhoneNumberUtil.getCountryCodeFromPhoneNumber(userPhoneNumber);
                this.countryPhoneCode = "+" + userPhoneNumber.getCountryCode();
                this.nationalNumber = ""+userPhoneNumber.getNationalNumber();
            }
        }
    }

    public boolean isValid(){
        return !ObjUtils.isNullOrEmpty(this.country) &&
                !ObjUtils.isNullOrEmpty(this.countryPhoneCode) && !ObjUtils.isNullOrEmpty(this.nationalNumber);
    }
}
