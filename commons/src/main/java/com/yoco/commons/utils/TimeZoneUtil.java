package com.yoco.commons.utils;

import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.services.UrlFetcher;
import com.yoco.commons.validations.Validator;
import lombok.extern.slf4j.Slf4j;

import java.time.Instant;
import java.time.ZoneId;
import java.time.format.TextStyle;
import java.time.zone.ZoneRulesException;
import java.util.*;

@Slf4j
public class TimeZoneUtil {

    public static final String OFFSET = "OFFSET";
    public static final String DISPLAY_NAME_SHORT = "DISPLAY_NAME_SHORT";
    public static final String DISPLAY_NAME_LONG = "DISPLAY_NAME_LONG";
    public static final String ZONE_DISPLAY_NAME_SHORT = "ZONE_DISPLAY_NAME_SHORT";
    public static final String ZONE_DISPLAY_NAME_LONG = "ZONE_DISPLAY_NAME_LONG";
    public static final String OFFSET_ZONE_DISPLAY_NAME_LONG = "OFFSET_ZONE_DISPLAY_NAME_LONG";

    public static final String INVALID_OFFSET = "Invalid offset";
    public static final String INVALID_ZONE_FORMAT = "Invalid ZoneFormat";
    public static final String INVALID_TIME_ZONE = "Invalid time zone";
    private static final String[] defaultList =  {"US/Samoa", "US/Hawaii", "Pacific/Marquesas", "US/Alaska", "US/Pacific", "US/Mountain", "US/Central", "US/Eastern", "America/Caracas", "Canada/Atlantic", "Canada/Newfoundland", "America/Buenos_Aires", "Brazil/East", "America/Sao_Paulo", "Atlantic/South_Georgia", "America/Scoresbysund", "Europe/London", "UTC", "Europe/Rome", "Africa/Johannesburg", "Asia/Riyadh", "Asia/Tehran", "Asia/Dubai", "Asia/Kabul", "Asia/Karachi", "Asia/Kolkata", "Asia/Kathmandu", "Asia/Dhaka", "Asia/Rangoon", "Asia/Bangkok", "Asia/Hong_Kong", "Asia/Kuala_Lumpur", "Asia/Shanghai", "Asia/Seoul", "Asia/Tokyo", "Australia/Adelaide", "Australia/Darwin", "Australia/Brisbane", "Australia/Melbourne", "Australia/Sydney", "Australia/LHI", "Pacific/Efate", "Pacific/Norfolk", "Asia/Magadan", "Pacific/Auckland", "Pacific/Fiji", "Pacific/Chatham", "Pacific/Apia", "Pacific/Kiritimati"};
    private static final String GOOGLE_TIMEZONE_API_URL = "https://maps.googleapis.com/maps/api/timezone/json?location={$coordinates}&timestamp={$timestamp}&key={$apikey}";
    private static final String GOOGLE_RESPONSE_ZONE_KEY = "timeZoneId";

    private TimeZoneUtil(){
        throw  new IllegalStateException("Utility class");
    }

    public static String getCurrentOffset(String zoneId) {

        var zone = ZoneId.of(zoneId);

        String offset = getZoneOffset(zone);

        offset =  "(GMT"+offset+")";

        String zoneDetails  =   zone.toString().replace("Etc/", "")+" ("+zone.getDisplayName(TextStyle.FULL_STANDALONE, Locale.ENGLISH )+")";

        return offset + " " +  zoneDetails;

    }

    public static Map<String, Object> getAllZoneIds() throws ZoneRulesException {

        Map<String, Object> sortedPosOsResult = new LinkedHashMap<>();
        Map<String, Object> sortedNegOsResult = new LinkedHashMap<>();

        List<String> zoneList  = new ArrayList<>(ZoneId.getAvailableZoneIds());

        Map<String, Object> posOsResult = new HashMap<>();
        Map<String, Object> negOsResult = new HashMap<>();

        for (String zoneId : zoneList) {

            if(Boolean.TRUE.equals(isZoneAllowed(zoneId ))) {

                var zone = ZoneId.of(zoneId);

                String offset = getZoneOffset(zone);

                offset = "GMT"+offset;

                String zoneDetails  =   zone.toString().replace("Etc/", "")+" ("+zone.getDisplayName(TextStyle.FULL_STANDALONE, Locale.ENGLISH )+")";

                if( offset.indexOf("+") != -1 ){
                    if( posOsResult.containsKey( offset )){
                        //key already existing
                        TreeSet<String> currZonesSet =   ( TreeSet<String>) posOsResult.get(offset);
                        currZonesSet.add(zoneDetails);

                        posOsResult.put(offset, currZonesSet);

                    }else{
                        TreeSet<String> newZonesSet = new TreeSet<>();
                        newZonesSet.add( zoneDetails);
                        posOsResult.put(offset,newZonesSet );
                    }
                }else{

                    if( negOsResult.containsKey( offset )){
                        //key already existing
                        TreeSet<String> currZonesSet =   ( TreeSet<String>) negOsResult.get(offset);
                        currZonesSet.add(zoneDetails);

                        negOsResult.put(offset, currZonesSet);

                    }else{
                        TreeSet<String> newZonesSet = new TreeSet<>();
                        newZonesSet.add( zoneDetails);
                        negOsResult.put(offset,newZonesSet );
                    }
                }
            }
        }

        //sorting positive offset zones list
        posOsResult.entrySet().stream()
                .sorted(Map.Entry.comparingByKey())
                .forEachOrdered(e -> sortedPosOsResult.put(e.getKey(), e.getValue()));

        //sorting negative offset zones list
        negOsResult.entrySet().stream()
                .sorted(Map.Entry.<String, Object>comparingByKey().reversed())
                .forEachOrdered(e -> sortedNegOsResult.put(e.getKey(), e.getValue()));

        sortedNegOsResult.putAll(sortedPosOsResult);

        return sortedNegOsResult;

    }

    public static String getTimeZoneID( String offset) {

        Validator.checkArgument( ObjUtils.isNullOrEmpty( offset ), INVALID_OFFSET);

        var               respZoneId         =   "UTC";

        Set<String>          offsetZones        = getTimeZoneForOffset( offset);

        if( !ObjUtils.isNull(offsetZones) && !offsetZones.isEmpty() ){

            for(String zone : offsetZones){

                if( Arrays.stream(defaultList).anyMatch( zone ::equalsIgnoreCase ) ) {
                    return zone;
                }
            }

        }
        return respZoneId;
    }

    public static String getZoneInFormat(String zone, String zoneFormat, Locale locale) throws ZoneRulesException{

        Validator.checkArgument( ObjUtils.isNullOrEmpty(zone), INVALID_TIME_ZONE);
        Validator.checkArgument( ObjUtils.isNullOrEmpty(zoneFormat), INVALID_ZONE_FORMAT);

        var  zoneId  =   ZoneId.of(zone);
        String  zoneInFormat = zone;

        locale = ObjUtils.isNull( locale )? Locale.ENGLISH : locale;

        switch ( zoneFormat){

            case OFFSET:
                zoneInFormat = String.format("GMT%s",getZoneOffset( zoneId));
                break;

            case DISPLAY_NAME_LONG:
                zoneInFormat =  zoneId.getDisplayName(TextStyle.FULL_STANDALONE, locale);
                break;

            case DISPLAY_NAME_SHORT:
                zoneInFormat =  zoneId.getDisplayName(TextStyle.SHORT_STANDALONE, locale);
                break;

            case ZONE_DISPLAY_NAME_LONG:
                zoneInFormat =  String.format("%s %s",zone, zoneId.getDisplayName(TextStyle.FULL_STANDALONE, locale));
                break;

            case ZONE_DISPLAY_NAME_SHORT:
                zoneInFormat =  String.format("%s %s",zone, zoneId.getDisplayName(TextStyle.SHORT_STANDALONE, locale));
                break;

            case OFFSET_ZONE_DISPLAY_NAME_LONG:
                String offset = getZoneOffset( zoneId);
                zoneInFormat = String.format("(GMT%s) %s (%s)", offset, zoneId, zoneId.getDisplayName(TextStyle.FULL_STANDALONE, locale));
                break;

            default:
                break;
        }

        return zoneInFormat;
    }

    private static String  getZoneOffset(ZoneId zoneId){

        return Instant.now().atZone( zoneId ).getOffset().getId().replace("Z", "+00:00");
    }

    public static Boolean isZoneAllowed( String zoneId ){
        return ( (zoneId.indexOf("/") >= 0) && (zoneId.indexOf("+") == -1) && (zoneId.indexOf("-") == -1)
                && (zoneId.indexOf("System") == -1) && (zoneId.indexOf("Etc/GMT0") == -1)
                && (zoneId.indexOf("Etc/Greenwich") == -1) && (zoneId.indexOf("Etc/UCT") == -1)
                && (zoneId.indexOf("Etc/Universal") == -1) && (zoneId.indexOf("Etc/Zulu") == -1)
                && (zoneId.indexOf("Asia/Calcutta") == -1) && (zoneId.indexOf("Asia/Katmandu") == -1)
                && (zoneId.indexOf("Asia/Dacca") == -1) && (zoneId.indexOf("Asia/Thimbu") == -1)
                && (zoneId.indexOf("Asia/Chungking") == -1) && (zoneId.indexOf("Asia/Macao") == -1)
                && (zoneId.indexOf("Asia/Ulan_Bator") == -1) && (zoneId.indexOf("Riyadh87") == -1)
                && (zoneId.indexOf("Riyadh88") == -1) && (zoneId.indexOf("Riyadh89") == -1));
    }

    private static Set<String> getTimeZoneForOffset ( String offset) {

        String      inputOffset =   convertOffset( offset ); //+05:30
        int         offsetMin   =   Integer.decode( offset );
        Set<String> respZones   =   new TreeSet<>();

        if( offsetMin > 0 || offsetMin < 0){

            Set<String> zoneIds       =   ZoneId.getAvailableZoneIds();

            for(String zoneId : zoneIds ){

                if( Boolean.TRUE.equals(isZoneAllowed( zoneId))){

                    var zone = ZoneId.of(zoneId);

                    String zoneOffset = getZoneOffset(zone);

                    if(inputOffset.equalsIgnoreCase(zoneOffset)){
                        respZones.add( zoneId );
                    }
                }
            }
        }

        return  respZones;

    }

    private static String convertOffset (String offset) {

        String      operator    =   '-' == offset.charAt(0) ? "-" : "+";

        int         offsetMin   =   Integer.decode( offset.replace("-", "") );
        int         hours       =   offsetMin / 60;
        int         min         =   offsetMin % 60;

        return  String.format("%s%02d:%02d", operator, hours, min);
    }

    public static String getTimezoneFromCoordinates(String latitude, String longitude){
        var timezone = "";
        try{
            if(!ObjUtils.isNullOrEmpty(latitude) && !ObjUtils.isNullOrEmpty(longitude)){
                var response = UrlFetcher.sendPostRequest(getTimezoneApiUrl(latitude,longitude),(Map<String,Object>)null,HeaderUtil.getContentJsonTypeHeader(),UrlFetcher.getHttpClientInstance());
                if(!ObjUtils.isNullOrEmpty(response) && !ObjUtils.isNull(response.get(GOOGLE_RESPONSE_ZONE_KEY))
                   && Boolean.TRUE.equals(isZoneAllowed((String)response.get(GOOGLE_RESPONSE_ZONE_KEY)))){
                        timezone = (String)response.get(GOOGLE_RESPONSE_ZONE_KEY);
                }
            }
        }catch (Exception e){
            log.info(e.getMessage());
        }
        return timezone;
    }

    public static String getTimezoneApiUrl(String latitude, String longitude){
        return GOOGLE_TIMEZONE_API_URL.replace("{$coordinates}",latitude.concat("," + longitude))
                .replace("{$timestamp}",DateUtil.getCurrentTime().toString())
                .replace("{$apikey}", CommonAppProperties.getGoogleMapsZoneApiKey());
    }

}
