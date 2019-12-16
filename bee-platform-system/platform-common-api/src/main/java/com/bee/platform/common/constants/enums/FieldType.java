package com.bee.platform.common.constants.enums;

import lombok.Getter;

/**
 * @description: PLC 漏斗中用户标识字段的类型 批次字段和漏斗字段
 * @author: junyang.li
 * @create: 2019-10-21 10:53
 **/
@Getter
public enum FieldType {

    /**
     * 标识字段为漏斗
     */
    FUNNEL(1,"漏斗"),
    /**
     * 标识字段为批次
     */
    BATCH(2,"料批"),

    ;

    private Integer key;

    private String value;

    FieldType(int key, String value) {
        this.key = key;
        this.value = value;
    }
    /**
     * @notes: 通过键获得对应的类型
     * @Author: junyang.li
     * @Date: 11:14 2019/10/21
     * @param key :
     * @return: com.bee.platform.common.constants.enums.FieldType
     */
    public static FieldType getType(int key){
        FieldType[] types=FieldType.values();
        for (FieldType item:types) {
            if(item.key== key){
                return item;
            }
        }
        return null;
    }
    /**
     * @notes: 通过键获得对应的值
     * @Author: junyang.li
     * @Date: 11:14 2019/10/21
     * @param key :
     * @return: com.bee.platform.common.constants.enums.FieldType
     */
    public static String getValueByKey(int key){
        FieldType type=FieldType.getType(key);
        return type==null?null:type.getValue();
    }
}
