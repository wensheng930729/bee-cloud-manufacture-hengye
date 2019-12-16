package com.bee.platform.cloud.si.manufacture.constants.enums;

/**
 * @Description 图片上传类型枚举
 * @author chenxm66777123
 * @Date 2019年5月9日
 * @version 1.0.0
 */
public class EnumUploadType {

    public enum UPLOAD_TYPE {
        IMAGE("0", "图片文件"), OFFICE("1", "OFFICE文件"),OTHER("2", "其他文件");
        private String key;
        private String value;

        UPLOAD_TYPE() {
        }

        UPLOAD_TYPE(String key, String value) {
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
