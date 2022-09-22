package com.yoco.account.modal;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.mock.web.MockHttpServletRequest;
import java.util.Map;

class GeoLocationDTOTest {
    @Test
    void constructor_noValidData_test(){
        GeoLocationDTO geoLocationDTO = new GeoLocationDTO(new MockHttpServletRequest());
        Assertions.assertEquals("",geoLocationDTO.getCountry());
        Assertions.assertEquals("",geoLocationDTO.getCity());
        Assertions.assertEquals("",geoLocationDTO.getLatitude());
        Assertions.assertEquals("",geoLocationDTO.getLongitude());
        Assertions.assertEquals("",geoLocationDTO.getRegion());
    }

    @Test
    void constructor_validCustomHeadersTest(){
        MockHttpServletRequest request = new MockHttpServletRequest();
        request.addHeader("X-Region","che");
        request.addHeader("X-City","ci");
        request.addHeader("X-Country","in");
        request.addHeader("X-LatLong","1.1,2.2");
        GeoLocationDTO geoLocationDTO = new GeoLocationDTO(request);
        Assertions.assertEquals("in",geoLocationDTO.getCountry());
        Assertions.assertEquals("ci",geoLocationDTO.getCity());
        Assertions.assertEquals("1.1",geoLocationDTO.getLatitude());
        Assertions.assertEquals("2.2",geoLocationDTO.getLongitude());
        Assertions.assertEquals("che",geoLocationDTO.getRegion());
    }

    @Test
    void constructor_validAppEngineHeadersTest(){
        MockHttpServletRequest request = new MockHttpServletRequest();
        request.addHeader("X-AppEngine-Region","che");
        request.addHeader("X-AppEngine-City","ci");
        request.addHeader("X-AppEngine-Country","in");
        request.addHeader("X-AppEngine-CityLatLong","1.1,2.2");
        GeoLocationDTO geoLocationDTO = new GeoLocationDTO(request);
        Assertions.assertEquals("in",geoLocationDTO.getCountry());
        Assertions.assertEquals("ci",geoLocationDTO.getCity());
        Assertions.assertEquals("1.1",geoLocationDTO.getLatitude());
        Assertions.assertEquals("2.2",geoLocationDTO.getLongitude());
        Assertions.assertEquals("che",geoLocationDTO.getRegion());
    }

    @Test
    void getGeoInfoMap_validTest(){
        MockHttpServletRequest request = new MockHttpServletRequest();
        request.addHeader("X-Region","che");
        request.addHeader("X-City","ci");
        request.addHeader("X-Country","in");
        request.addHeader("X-LatLong","1.1,2.2");
        GeoLocationDTO geoLocationDTO = new GeoLocationDTO(request);
        Map<String,Object> actual = geoLocationDTO.getGeoInfoMap();
        Assertions.assertEquals(Map.of("country","in","city","ci","region","che","latlong","1.1,2.2"),actual);
    }
}
