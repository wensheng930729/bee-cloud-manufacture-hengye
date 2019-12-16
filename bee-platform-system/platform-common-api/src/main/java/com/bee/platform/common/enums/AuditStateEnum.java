package com.bee.platform.common.enums;

import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * 
 * @author dell
 * 产品审核枚举类
 *
 */
@Getter
@NoArgsConstructor
public enum AuditStateEnum {

    NOT_PASSED(0,"未通过"),PASSED(1,"已通过"),ON_AUDIT(2,"待审核"),ALL(3,"全部类型");

    private Integer key;

    private String value;

    AuditStateEnum(Integer key,String value){
        this.key=key;
        this.value=value;
    }
}
