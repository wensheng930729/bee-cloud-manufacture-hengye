package com.bee.platform.common.constants.enums;

/**
 * @ClassName EnumEnterpriseLevel
 * @Description 公司层级
 */

public class EnumEnterpriseLevel {

    public enum levelNode{

        first_level(1,"一级"),second_level(2,"二级"),third_level(3,"三级")
        ,fourth_level(4,"四级"),fifth_level(5,"五级")
        ;

        levelNode() {
        }

        levelNode(Integer key, String value) {
            this.key = key;
            this.value = value;
        }

        private Integer key;
        private String value;

        public Integer getKey() {
            return key;
        }

        public void setKey(Integer key) {
            this.key = key;
        }

        public String getValue() {
            return value;
        }

        public void setValue(String value) {
            this.value = value;
        }
    }
}
