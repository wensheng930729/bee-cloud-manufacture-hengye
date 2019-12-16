package com.bee.platform.common.constants.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * 后台操作类型及操作结果
 * @author dell
 *
 */
public class EnumOperateLog {

	/**
	 * 产品及角色申请操作类型
	 * @author dell
	 *
	 */
    @Getter
    @NoArgsConstructor
    @AllArgsConstructor
    public enum AppOperateType{
        apply(0,"申请开通产品"),audit(1,"审核申请");
        private Integer key;
        private String value;

    }
    
    
    /**
	 * 产品及角色审核执行结果
	 * @author dell
	 *
	 */
    @Getter
    @NoArgsConstructor
    @AllArgsConstructor
    public enum executResult {
    	NOT_PASSED(0,"未通过"),PASSED(1,"已通过"),ON_AUDIT(2,"待审核");
        private Integer key;
        private String value;
    	
    }
}
