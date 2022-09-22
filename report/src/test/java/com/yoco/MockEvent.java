package com.yoco;

import com.yoco.commons.modal.report.ReportsDTO;
import com.yoco.commons.utils.JsonUtil;

import java.util.Map;

public class MockEvent {

    private static final String YOCO_EVENT_JSON = "{\"id\":\"ba5b5d45-40cb-4635-8deb-9b40796e3173\",\"contactID\":\"669fd9ad-f3d9-4ec0-aee7-48f617324c65\",\"emailID\":\"gajendran.mohan@anywhere.co\",\"accountID\":\"91dfed2f-d29f-4302-89ee-341e9364b941\",\"status\":\"MANUAL_CLOCKED_OUT\",\"payrollStatus\":\"\",\"inIP\":\"182.79.18.68\",\"inSource\":\"aw\",\"inTime\":1651654645560,\"outIP\":\"182.79.18.68\",\"outSource\":\"aw\",\"outTime\":1651664108540,\"clockInMessage\":\"{\\\"country\\\":\\\"IN\\\",\\\"city\\\":\\\"chennai\\\",\\\"latlong\\\":\\\"13.082680,80.270718\\\",\\\"region\\\":\\\"tn\\\"}\",\"clockOutMessage\":\"{\\\"country\\\":\\\"IN\\\",\\\"city\\\":\\\"chennai\\\",\\\"latlong\\\":\\\"13.082680,80.270718\\\",\\\"region\\\":\\\"tn\\\"}\",\"subStatus\":\"\",\"projectID\":\"91dfed2f-d29f-4302-89ee-341e9364b941-1166394532682393\",\"projectName\":\"IT works\",\"start\":\"2022-05-04T02:27:25 PM+0530\",\"stop\":\"2022-05-04T05:05:08 PM+0530\",\"taskSource\":\"\",\"taskDescription\":\"\",\"taskID\":\"\",\"deleted\":false}";
    private static final String RAW_EVENT = "{\"brand\":\"d56194e1-b98b-4068-86f8-d442777d2a16\",\"cost\":0,\"queryableMeta\":{\"payrollStatus\":\"DEFAULT_PAYROLL\",\"status\":\"MANUAL_CLOCKED_OUT\"},\"paymentStatus\":\"DEFAULT_PAYROLL\",\"source\":\"aw\",\"location\":{\"clockOutMessage\":{\"ip\":\"2401:4900:3603:b44a:b866:a96c:a909:dc98\",\"info\":{\"country\":\"IN\",\"city\":\"chennai\",\"latlong\":\"13.082680,80.270718\",\"region\":\"tn\"}},\"clockInMessage\":{\"ip\":\"2401:4900:3603:b44a:902c:6f2c:71e0:9697\",\"info\":{\"country\":\"IN\",\"city\":\"chennai\",\"latlong\":\"13.082680,80.270718\",\"region\":\"tn\"}}},\"metaData\":{\"outSource\":\"aw\",\"status\":\"MANUAL_CLOCKED_OUT\"},\"bookingId\":\"yng4uzot\",\"isAllDay\":false,\"id\":\"c07b7576-ce1d-4cf1-8763-614a51f3f100\",\"merchant\":\"123\",\"calendar\":\"123\",\"startTime\":1651244601363,\"endTime\":1651247565263,\"startDateTime\":\"2022-04-29T15:03:21:363Z\",\"endDateTime\":\"2022-04-29T15:52:45:263Z\",\"maxSeats\":0,\"service\":[],\"consumer\":[],\"provider\":[\"111\"],\"resource\":[],\"type\":\"EVENT\",\"createdTime\":1651247565290,\"updatedTime\":1651247565294,\"isExternal\":false,\"isDeleted\":false,\"log\":{\"changedProperties\":{}}}";

    public static Map<String, Object> getRawEvent() {
        return JsonUtil.convertJsonToMap(RAW_EVENT);
    }

    public static Map<String, Object> getYoCoEvent() {
        return JsonUtil.convertJsonToMap(YOCO_EVENT_JSON);
    }

    public static ReportsDTO getEventInReportsDTO() {
        return new ReportsDTO(JsonUtil.convertJsonToMap(RAW_EVENT), "test@gmail.com", "Asia/Kolkata");
    }

}
