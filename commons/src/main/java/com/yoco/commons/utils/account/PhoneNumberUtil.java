package com.yoco.commons.utils.account;

import com.google.i18n.phonenumbers.Phonenumber;
import com.yoco.commons.utils.ObjUtils;

public class PhoneNumberUtil {
    private PhoneNumberUtil(){}
    public static Phonenumber.PhoneNumber extractUserPhoneNumber(String phoneNumber, String countryCode){
        try{
            return com.google.i18n.phonenumbers.PhoneNumberUtil.getInstance().parseAndKeepRawInput(phoneNumber,countryCode.toUpperCase());
        }catch (Exception e){
            return null;
        }
    }

    public static String getCountryCodeFromPhoneNumber(Phonenumber.PhoneNumber phoneNumber){
        try{
            return com.google.i18n.phonenumbers.PhoneNumberUtil.getInstance().getRegionCodeForNumber(phoneNumber);
        }catch (Exception e){
            return null;
        }
    }

    public static boolean isValidPhoneNumber(Phonenumber.PhoneNumber number){
        return !ObjUtils.isNull(number) && number.hasCountryCode() && number.hasNationalNumber();
    }
}
