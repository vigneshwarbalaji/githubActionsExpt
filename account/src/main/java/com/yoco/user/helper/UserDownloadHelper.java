package com.yoco.user.helper;

import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.utils.AccountUtil;
import com.yoco.commons.utils.ObjUtils;
import java.util.List;

public class UserDownloadHelper {
    public static final String STATUS_ACTIVE = "active";
    public static final String STATUS_INACTIVE = "inactive";
    public static final String CSV_TERMINATOR = "\r\n";
    private UserDownloadHelper(){}

    public static Object getIsDeleteQueryValue(String status){
        if(STATUS_ACTIVE.equalsIgnoreCase(status)){
            return Boolean.FALSE;
        }else if(STATUS_INACTIVE.equalsIgnoreCase(status)){
            return Boolean.TRUE;
        }else{
            return null;
        }
    }

    public static String generateStaffDataCsv(List<PeopleRelationJDO> staffList,String outputFields){
        boolean shouldIncludeStatusField = isFieldIncludedInOutputFields(outputFields,"status");
        boolean shouldIncludeEmployeeIdField = isFieldIncludedInOutputFields(outputFields,"empID");
        var csvReport = new StringBuilder();
        csvReport.append(getUserReportHeader(shouldIncludeStatusField,shouldIncludeEmployeeIdField));
        if(shouldIncludeStatusField && shouldIncludeEmployeeIdField){
            return getReportStringForUserEmailStatusEmpId(staffList,csvReport);
        }else if(shouldIncludeEmployeeIdField){
            return getReportStringForUserEmailEmpID(staffList,csvReport);
        }else if(shouldIncludeStatusField){
            return getReportStringForUserEmailStatus(staffList,csvReport);
        }else {
            return getReportStringForUserEmail(staffList,csvReport);
        }
    }

    public static String generateStaffDataCsvFileName(String accountID,Boolean staffStatus){
        String fileName = "StaffDataReport_" + AccountUtil.getCompanyDisplayName(accountID) + "_";
        if(Boolean.TRUE.equals(staffStatus)){
            fileName += "Inactive.csv";
        }else if(Boolean.FALSE.equals(staffStatus)){
            fileName += "Active.csv";
        }else{
            fileName += "ActiveInactive.csv";
        }
        return fileName;
    }

    public static boolean isFieldIncludedInOutputFields(String outputFields, String field){
        try{
            return !ObjUtils.isNullOrEmpty(field) && List.of(outputFields.split(",")).contains(field);
        }catch (Exception e){
            return false;
        }
    }

    public static String getUserReportHeader(boolean includeStatusField, boolean includeEmployeeIdField){
        var reportHeader = "Email";
        if(includeStatusField){
            reportHeader += ",Status";
        }
        if(includeEmployeeIdField){
            reportHeader += ",EmployeeID";
        }
        return reportHeader + CSV_TERMINATOR;
    }

    public static String getReportStringForUserEmailStatusEmpId(List<PeopleRelationJDO> staffList, StringBuilder reportBuilder){
        staffList.forEach(userPro -> reportBuilder.append(userPro.getEmailID())
                .append(getStaffStatusForUserReport(userPro.isDelete()))
                .append(getEmployeeIdForUserReport(userPro.getLogin()))
                .append(CSV_TERMINATOR));
        return reportBuilder.toString();
    }

    public static String getReportStringForUserEmailEmpID(List<PeopleRelationJDO> staffList,StringBuilder reportBuilder){
        staffList.forEach(userPro -> reportBuilder.append(userPro.getEmailID())
                .append(getEmployeeIdForUserReport(userPro.getLogin()))
                .append(CSV_TERMINATOR));
        return reportBuilder.toString();
    }

    public static String getReportStringForUserEmailStatus(List<PeopleRelationJDO> staffList,StringBuilder reportBuilder){
        staffList.forEach(userPro -> reportBuilder.append(userPro.getEmailID())
                .append(getStaffStatusForUserReport(userPro.isDelete()))
                .append(CSV_TERMINATOR));
        return reportBuilder.toString();
    }

    public static String getReportStringForUserEmail(List<PeopleRelationJDO> staffList,StringBuilder reportBuilder){
        staffList.forEach(userPro -> reportBuilder.append(userPro.getEmailID())
                .append(CSV_TERMINATOR));
        return reportBuilder.toString();
    }

    public static String getEmployeeIdForUserReport(String empID){
        return "," + (ObjUtils.isNullOrEmpty(empID) ? "-" : empID);
    }

    public static String getStaffStatusForUserReport(boolean isDeleted ){
        return "," + (isDeleted ? "Inactive" : "Active");
    }
}
