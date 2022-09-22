package com.yoco.user.service;

import com.yoco.MockPRO;
import com.yoco.commons.dataservices.impl.UserImpl;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.modal.date.RangeInfoDTO;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.utils.TimeZoneUtil;
import com.yoco.commons.utils.UserPROUtil;
import com.yoco.user.enums.USER_ERROR_MESSAGE;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;

class UserDateTimeServiceTest {
    @Test
    void getRangeInfo_nullUserPro_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class)){
            userPROUtilMockedStatic.when(()->UserPROUtil.validateAndExtractUserPRO("234", "123")).thenReturn(null);
           new UserDateTimeService().getRangeInfo("234","123","current_week","","");
        }catch (Exception e){
            Assertions.assertEquals(USER_ERROR_MESSAGE.USER_NOT_FOUND.value(),e.getMessage());
        }
    }

    @Test
    void getRangeInfo_validCurrentWeekRange_test(){
        try(MockedStatic<UserPROUtil> userPROUtilMockedStatic = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<DateUtil> dateUtilMockedStatic = Mockito.mockStatic(DateUtil.class)){
            PeopleRelationJDO userPro = new PeopleRelationJDO();
            userPro.setTimeZone("Asia/Kolkata");
            userPROUtilMockedStatic.when(()->UserPROUtil.validateAndExtractUserPRO("234", "123")).thenReturn(userPro);
            RangeInfoDTO rangeInfoDTO = new RangeInfoDTO();
            dateUtilMockedStatic.when(()->DateUtil.getRangeDetails("Asia/Kolkata","current_week","","","234")).thenReturn(rangeInfoDTO);
            RangeInfoDTO actual = new UserDateTimeService().getRangeInfo("234","123","current_week","","");
            Assertions.assertEquals(rangeInfoDTO,actual);
        }
    }

    @Test
    void checkDSTorNot_same_offset_check() {

        try(MockedStatic<UserPROUtil> userPROUtilMock = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<TimeZoneUtil> timeZoneUtilMock = Mockito.mockStatic(TimeZoneUtil.class)) {

            userPROUtilMock.when(()-> UserPROUtil.getUserPro(anyString(),anyString())).thenReturn(MockPRO.getMockPRO());
            timeZoneUtilMock.when(()->TimeZoneUtil.getCurrentOffset(anyString())).thenReturn("(GMT+05:30) Asia/Kolkata (India Time)");

            Map<String, Object> resp = new UserDateTimeService().checkDSTorNot("accountID", "contactID");
            Assertions.assertTrue(!ObjUtils.isNullOrEmpty(resp));

        }
    }

    @Test
    void checkDSTorNot_differnt_offset_check() {

        try(MockedStatic<UserPROUtil> userPROUtilMock = Mockito.mockStatic(UserPROUtil.class);
            MockedStatic<TimeZoneUtil> timeZoneUtilMock = Mockito.mockStatic(TimeZoneUtil.class);
            MockedConstruction<UserImpl> mock = Mockito.mockConstruction(UserImpl.class, (userImplMock, context) -> {
                Mockito.when(userImplMock.savePro(any(PeopleRelationJDO.class))).thenReturn(MockPRO.getMockPRO());
            })) {

            userPROUtilMock.when(()-> UserPROUtil.getUserPro(anyString(),anyString())).thenReturn(MockPRO.getMockPRO());
            timeZoneUtilMock.when(()->TimeZoneUtil.getCurrentOffset(anyString())).thenReturn("(GMT-07:00) US/Pacific (Pacific Time)");

            Map<String, Object> resp = new UserDateTimeService().checkDSTorNot("accountID", "contactID");
            Assertions.assertTrue(!ObjUtils.isNullOrEmpty(resp));

        }
    }
}
