package com.bee.platform.common.enums;



/**
 * 产品审核消息提示枚举
 * @author dell
 *
 */
public class EnumEnterpriseApp {

	public enum CheckMsgType {
		REFUSED(0, "拒绝"), PASSED(1, "通过");
		private Integer key;
		private String value;

		CheckMsgType() {
		}

		CheckMsgType(Integer key, String value) {
			this.key = key;
			this.value = value;
		}

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
