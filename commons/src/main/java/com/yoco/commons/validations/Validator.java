package com.yoco.commons.validations;

import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.commons.utils.ObjUtils;
import org.apache.commons.text.StringEscapeUtils;
import org.apache.commons.validator.routines.EmailValidator;
import org.jsoup.Jsoup;
import org.jsoup.safety.Safelist;

import java.net.URL;
import java.time.ZoneId;
import java.util.Arrays;
import java.util.Map;
import java.util.regex.Pattern;

public class Validator {

    private Validator(){}

    private static final EmailValidator emailValidator = EmailValidator.getInstance();
    private static final String[] BLACKLISTED_PROTOCOLS = new String[]{"https://","http://","www.","ftp://"};

    public static void checkArgument(boolean expression, String message) {
        if(expression){
            throw new IllegalArgumentException(message);
        }
    }

    public static boolean isValidEmail(String email) {
        if (ObjUtils.isNullOrEmpty(email)) {
            return false;
        }

        var firstChar = "^[a-zA-Z0-9]";

        var firstCharPattern = Pattern.compile(firstChar);
        var firstCharMatcher = firstCharPattern.matcher(""+email.charAt(0));

        return firstCharMatcher.matches() && emailValidator.isValid(email);
    }

    public static boolean isFullDomain( String email){

        final var restrictedDomains = new String[]{"@a-cti.com", "@anywhere.co", "@full.io", "@full.co",
                "@adaptavantcloud.com", "@answerconnect.co.uk", "@anywhereworks.com", "@anywherehelp.io", "@adaptavant.com",
                "@answerconnect.com", "@answerforce.com", "@lexreception.com", "@setmore.com", "@hellosells.com", "@chatsupport.co",
                "@wellreceived.com", "@teleport.video", "@anywhereworks.co"};

        if( ObjUtils.isNullOrEmpty(email) || email.endsWith("@a-cti.com") || email.endsWith("@anywhere.co") || email.endsWith("@full.io")|| email.endsWith("@full.co")
                || email.endsWith("@adaptavantcloud.com") || email.endsWith("@answerconnect.co.uk") || email.endsWith("@anywhereworks.com") || email.endsWith("@anywherehelp.io") || email.endsWith("@adaptavant.com") ||
                email.endsWith("@answerconnect.com") || email.endsWith("@answerforce.com") || email.endsWith("@lexreception.com") || email.endsWith("@setmore.com") || email.endsWith("@hellosells.com") || email.endsWith("@chatsupport.co")
                || email.endsWith("@wellreceived.com") || email.endsWith("@teleport.video") || email.endsWith("@anywhereworks.co")) {

            return true;
        }
        String matchingFullDomain = Arrays.stream(restrictedDomains).filter(email::endsWith).findFirst().orElse(null);
        return !ObjUtils.isNullOrEmpty(matchingFullDomain);
    }

    public static boolean isValidSHA1(String str) {
        return str.matches("^[a-fA-F0-9]{40}$");
    }

    public static String sanitizeText(String inputText){
        if(ObjUtils.isNullOrEmpty(inputText)){
            return "";
        }
        String protocolRemovedInputText = inputText;
        for(String blacklistedString : BLACKLISTED_PROTOCOLS){
            protocolRemovedInputText = protocolRemovedInputText.replace(blacklistedString,"");
        }
        String sanitizedInputText = Jsoup.clean(protocolRemovedInputText, Safelist.none());
        String[] periodSplitTextArray = sanitizedInputText.split("\\.");
        return String.join(". ", Arrays.stream(periodSplitTextArray).map(String::trim).toArray(String[] :: new));
    }

    public static String sanitizeUrl(String url){
        if(!Validator.isValidUrl(url)){
            return "";
        }
        return Jsoup.clean(url, Safelist.none());
    }

    public static String sanitizeEmail(String email){
        if(!Validator.isValidEmail(email)){
            return "";
        }
        return Jsoup.clean(email, Safelist.none());
    }

    public static int validateAndReturnLimit(Integer limit, int defaultLimit){
        if(limit == null || limit < 1){
            return defaultLimit;
        }
        return limit;
    }

    public static boolean isValidCoordinates(String latitude, String longitude){
        try{
            return Integer.parseInt(latitude)!=0 && Integer.parseInt(longitude)!=0;
        }catch (Exception e){
            return false;
        }
    }

    public static boolean isValidTimeZone(String timezone){
        try{
            ZoneId.of(timezone);
            return true;
        }catch (Exception e){
            return false;
        }
    }

    public static boolean isValidLongNumber(String number){
        try{
            return Long.parseLong(number) > 0;
        }catch (Exception e){
            return false;
        }
    }

    public static boolean isValidUrl(String url){
        try{
            new URL(url).toURI();
            return true;
        }catch (Exception e){
            return false;
        }
    }

    public static String unescapeHtml(String message){
        return StringEscapeUtils.unescapeHtml4(sanitizeText(message));
    }

    public static Map<String,Object> validateAndExtractPayload(String payload){
        checkArgument(ObjUtils.isNullOrEmpty(payload), COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value());
        Map<String, Object> payloadMap = JsonUtil.convertJsonToMap(payload);
        checkArgument(ObjUtils.isNullOrEmpty(payloadMap),COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value());
        return payloadMap;
    }
}
