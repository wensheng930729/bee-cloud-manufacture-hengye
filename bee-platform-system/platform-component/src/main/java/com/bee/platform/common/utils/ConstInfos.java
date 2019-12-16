package com.bee.platform.common.utils;

import lombok.Getter;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

/**
 * Created by CrazyMouse on 2016/12/12.
 */
@Component
public class ConstInfos {
    //单个账户导入
    public static String synchronzizeUrl = "https://console.tim.qq.com/v4/im_open_login_svc/account_import";
    public static String userSig = "eJxljkFPhDAYRO-8ioaz0RaoVhMPKMQlwWUNKMKlqduCnxsKgS6yMf53FTeRxLm*l5n5sBBCdhanp2K7bffacHPolI2ukI3tkz-YdSC5MNzt5T*opg56xUVlVD9DQil1MF46IJU2UMHReAPdABeyAb2QBrnj89Jvi-ddQS6cS7ZUoJ7hffh4Gz0EcTolOVvt24Hlz61RRV4HIh2jM7gJ8GtL3QN5KpMhk3d1VOfVe8h8kSWb0juHopli3ycxHdc43KziYmdKIwNYv0AVXS8mDTTqeIgR6njUXR4aVT9Aq2fBwYQSx8U-sa1P6wshkV*z";
    public static String admin = "jinmi_admin";
    public static long sdkappid = 1400017298;
    public static String contenttype = "json";
    public static String apn = "1";
    /** 15,552,000*/
    public static final long time = 15552000L;

    public static String privStr = "-----BEGIN PRIVATE KEY-----\n" +
            "MIGEAgEAMBAGByqGSM49AgEGBSuBBAAKBG0wawIBAQQgWXM5aTXPOLdrogXqEgZy\n" +
            "q5jK5gNfBex2SOV1XwjqC4GhRANCAARTGAQl3F59HGYPhPKI7bzQ3FjhyUCm1ikX\n" +
            "hahXo5H54H1WAh6Iq2QDaJ1EryCHoh/RQByc6t2W75EBeK6VEsLQ\n" +
            "-----END PRIVATE KEY-----";
    public static String pubStr = "-----BEGIN PUBLIC KEY-----\n" +
            "MFYwEAYHKoZIzj0CAQYFK4EEAAoDQgAEUxgEJdxefRxmD4TyiO280NxY4clAptYp\n" +
            "F4WoV6OR+eB9VgIeiKtkA2idRK8gh6If0UAcnOrdlu+RAXiulRLC0A==\n" +
            "-----END PUBLIC KEY-----";

//    public static final String imageLocalpath = "/Users/CrazyMouse/Desktop/temp/";
//    本地测试图片缓存路径
//    public static final String imageLocalpath = "G:/temp/";
//    3.8测试服图片缓存路径
//    public static final String imageLocalpath = "/home/bee/project/temp/";
//    3.7和正式服图片缓存路径
    public static String imageLocalpath;

    public static final String imageNormalUrl = "http://beesrv-1252637635.file.myqcloud.com/defaultHead.png";

    /** 必填:短信模板-可在短信控制台中找到*/
    public static final String SMS_REGISTER = "SMS_25010454";
    public static final String sms_invite = "SMS_48345059";

    //企业审核类型
    public enum REVIEW {
       NO, OK, UPDATE, NEW, REVOKE
    }
    //资讯类型
    public enum NEWS_TYPE {
        HEADLINE, //头条
        FLASH, //快讯
        ANALYSIS_AND_COMMENT, //分析评论
        AI,  //人工智能
        OTHERINFO //其它资讯
    }

    //学习指南类型 使用指南 0 内部培训 1 平台信息 2 规章制度 3
    public enum LEARN_TYPE {
        USE, //使用指南
        TRAINING, //内部培训
        PLATFORM_INFO, //平台信息
        RULES  //规章制度
    }

    @Getter
    public enum ARTICLE_TYPE {
        /**
         * 企业发文
         */
        ORG_ARTICLE(0),
        /**
         * 规章制度
         */
        RULES_AND_REGULATIONS(1),
        /**
         * 内部咨询
         */
        ORG_CULTURE(2),
        /**
         * 督察通报
         */
        HONOR_ROLL(3),
        /**
         * 所有
         */
        All(10);

        private Integer key;

        ARTICLE_TYPE(){

        }

        ARTICLE_TYPE(Integer key){
            this.key=key;
        }
        /**
         * @notes 通过key获得类型
         * @Author junyang.li
         * @Date 11:09 2019/4/1
         **/
        public static ARTICLE_TYPE getType(int key){
            ARTICLE_TYPE[] tyeps=ARTICLE_TYPE.values();
            for (ARTICLE_TYPE item:tyeps) {
                if(item.getKey()==key){
                    return item;
                }
            }
            return null;
        }

    }
    
    public enum ManageEnum {
        PERSON(0,"成员"),MANAGE(1,"管理员");

        private Integer key;

        private String val;

        ManageEnum(){

        }

        ManageEnum(Integer key, String val){
            this.key=key;
            this.val=val;
        }

        public static ManageEnum getManage(Integer key){
            if(key==null){
                return null;
            }
            ManageEnum[] enums=ManageEnum.values();
            for (ManageEnum item: enums) {
                if(item.getKey().equals(key)){
                    return item;
                }
            }
            return null;
        }

        public static String  getValue(Integer key){
            ManageEnum manageEnum=getManage(key);
            return manageEnum==null?null:manageEnum.val;
        }
        public Integer getKey() {
            return key;
        }

        public void setKey(Integer key) {
            this.key = key;
        }

        public String getVal() {
            return val;
        }

        public void setVal(String val) {
            this.val = val;
        }
    }

    //序列值
    public enum Sequence {
        workOrdersSeq("workOrdersSeq", "工单编号序列")
        ;
        private String key;
        private String value;

        Sequence() {
        }

        Sequence(String key, String value) {
            this.key = key;
            this.value = value;
        }

        public void setKey(String key) {
            this.key = key;
        }

        public String getKey() {
            return key;
        }

        public void setValue(String value) {
            this.value = value;
        }

        public String getValue() {
            return value;
        }
    }

}
