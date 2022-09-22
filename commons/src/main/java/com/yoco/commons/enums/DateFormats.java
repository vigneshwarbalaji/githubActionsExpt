package com.yoco.commons.enums;

import com.yoco.commons.utils.ObjUtils;

public enum DateFormats {

    DD_MMM_YYYY {
        @Override
        public String toString() {
            return "dd-MMM-yyyy";
        }
    },D_MM_YYYY {
        @Override
        public String toString() {
            return "M/d/yyyy";
        }
    }, HH_MM_SS_AA {
        @Override
        public String toString() {
            return "hh:mm:ss aa";
        }
    }, HH_MM_SS_A {
        @Override
        public String toString() { return "hh:mm:ss a"; }
    },YYYY_MM_DD {
        @Override
        public String toString() {
            return "yyyy-MM-dd";
        }
    }, YYYY_MM_DD_T_HH_MM_SSZ {
        @Override
        public String toString() {
            return "yyyy-MM-dd'T'HH:mm:ssZ";
        }
    }, YYYY_MM_DD_T_HH_MM_SS{
        @Override
        public String toString() {
            return "yyyy-MM-dd'T'HH:mm:ss";
        }
    },YYYY_MM_DD_T_HH_MM_SS_A_Z(){
        @Override
        public String toString() {
            return "yyyy-MM-dd'T'hh:mm:ss aZ";
        }
    },  YYYY_MM_DD_HH_MM_SS_A(){
        @Override
        public String toString() {
            return "yyyy-MM-dd'T'hh:mm:ss a";
        }
    },ZZZZ {
        @Override
        public String toString() {
            return "zzzz";
        }
    }, DD_MMM_YYYY_HH_MM_SS_A {
        @Override
        public String toString() {
            return "dd-MMM-yyyy hh:mm:ss a";
        }
    }, MM_DD_YYYY_HH_MM_SS_A {
        @Override
        public String toString() {
            return "MM/dd/yyyy hh:mm:ss a";
        }
    }, DD_MM_YYYY {
        @Override
        public String toString() {
            return "dd-mm-yyyy";
        }
    }, MM_DD_YYYY {
        @Override
        public String toString() {
            return "MM/dd/yyyy";
        }
    }, EEE_MMM_DD {
        @Override
        public String toString() { return "EEE, MMM dd"; }
    },E_MMM_DD_YYYY{
        @Override
        public String toString() { return "EEE, MMM dd yyyy"; }
    }, YYYY_MM_DD_MMM_EEE {
        @Override
        public String toString() { return "yyyy-MM-dd~MMM~EEE"; }
    },E_MMM_D_YYYY{
        @Override
        public String toString() {
            return "E, MMM d yyyy";
        }
    }, ZULU {
        @Override
        public String toString() {
            return "yyyy-MM-dd'T'HH:mm:ss:SSSXXX";}
    };

    public static boolean isValidDateFormat(String dateFormatString){
        for (DateFormats dateFormat : values()) {
            if (dateFormat.toString().equalsIgnoreCase(dateFormatString)) {
                return true;
            }
        }
        return false;
    }

    public static String validateAndReturnDateFormatString(String dateFormatString){
        return isValidDateFormat(dateFormatString) ? dateFormatString : DateFormats.MM_DD_YYYY_HH_MM_SS_A.toString();
    }

    public static String validateAndReturnDateFormatString(DateFormats dateFormat){
        return ObjUtils.isNull(dateFormat) ?  DateFormats.MM_DD_YYYY_HH_MM_SS_A.toString() : dateFormat.toString();
    }

    public static DateFormats parseAndGetDateFormat(String dateFormatString) {
        for (DateFormats dateFormat : values()) {
            if (dateFormat.toString().equalsIgnoreCase(dateFormatString)) {
                return dateFormat;
            }
        }
        return DateFormats.DD_MMM_YYYY_HH_MM_SS_A;
    }
}
