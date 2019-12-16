package com.bee.platform.common.enums;

/**
 * Description t_config 表中对应的配置键的枚举
 * 
 * @author zhigang.zhou
 * @date 2018年11月27日 下午4:26:14
 * @version V 1.0
 */
public enum ConfigKeyEnum {

	IS_ENABLESYSCODECACHE_KEY("supplychainfinance_is_enable_syscodecache_v1.0", "是否开启码表缓存的键");

	public String codeValue;

	public String desc;

	private ConfigKeyEnum(String codeValue, String desc) {
		this.codeValue = codeValue;
		this.desc = desc;
	}

	public String getCodeValue() {
		return codeValue;
	}

	public void setCodeValue(String codeValue) {
		this.codeValue = codeValue;
	}

	public String getDesc() {
		return desc;
	}

	public void setDesc(String desc) {
		this.desc = desc;
	}

	/**
	 * 在枚举里面 定义一个静态方法
	 */
	public static boolean contains(String type) {
		for (ConfigKeyEnum configKeyEnum : ConfigKeyEnum.values()) {
			// 通过枚举名称判断
			if (configKeyEnum.name().equals(type)) {
				return true;
			}
		}
		return false;
	}

}
