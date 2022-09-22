package com.yoco.account.modal;

import com.yoco.commons.utils.HeaderUtil;
import com.yoco.commons.utils.ObjUtils;
import lombok.Data;
import javax.servlet.http.HttpServletRequest;
import java.util.HashMap;
import java.util.Map;

@Data
public class GeoLocationDTO {
    private String city = "";
    private String country = "";
    private String region = "";
    private String latitude = "";
    private String longitude = "";

    public static final String COUNTRY_KEY = "country";
    public static final String CITY_KEY = "city";
    public static final String LAT_LONG_KEY = "latlong";
    public static final String REGION_KEY = "region";

    public GeoLocationDTO(){}

    public GeoLocationDTO(HttpServletRequest request){
        this.region = HeaderUtil.extractRegionFromRequest(request);
        this.city = HeaderUtil.extractCityFromRequest(request);
        this.country = HeaderUtil.extractCountryFromRequest(request);
        this.extractAndSetCoordinates(HeaderUtil.extractLatLongFromRequest(request));
    }

    public void extractAndSetCoordinates(String coordinates){
        if(!ObjUtils.isNullOrEmpty(coordinates)){
            String[] latLong = coordinates.split(",");
            this.latitude = latLong[0];
            this.longitude = latLong[1];
        }
    }



    public Map<String,Object> getGeoInfoMap(){
        Map<String,Object> geoInfoMap = new HashMap<>();
        geoInfoMap.put(COUNTRY_KEY,this.getCountry());
        geoInfoMap.put(CITY_KEY,this.getCity());
        geoInfoMap.put(REGION_KEY,this.getRegion());
        geoInfoMap.put(LAT_LONG_KEY,this.getLatitude().concat("," + this.getLongitude()));
        return geoInfoMap;
    }
}
