package com.bee.platform.cloud.si.manufacture.constants.enums;

import lombok.Getter;

/**
 * @description: 库存盘点，盘点类型
 * @author: junyang.li
 * @create: 2019-11-25 16:49
 **/
@Getter
public enum  EnumInventoryType {
    /**
     * 盘点类型
     */
    ALL(1,"全盘"),

    PRODUCT_TYPE(2,"盘点产品分类"),

    PRODUCT(3,"产品盘点"),

    WAREHOUSE(4,"盘点仓库"),
    ;

    private Integer code;

    private String desc;

    EnumInventoryType(Integer code, String desc) {
        this.code = code;
        this.desc = desc;
    }
    /**
     * @notes: 通过盘点分类code获得对应的枚举对象
     * @Author: junyang.li
     * @Date: 9:56 2019/11/26
     * @param code :
     * @return: com.bee.platform.cloud.si.manufacture.constants.enums.EnumInventoryType
     */
    public static EnumInventoryType getInventoryTypeByCode(Integer code){
        if(code!=null){
            EnumInventoryType[] types=EnumInventoryType.values();
            for (EnumInventoryType item:types) {
                if(item.getCode().equals(code)){
                    return item;
                }
            }
        }
        return null;
    }
}
