package com.bee.platform.common.entity;


import com.baomidou.mybatisplus.activerecord.Model;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;
import org.springframework.format.annotation.DateTimeFormat;

import java.io.Serializable;
import java.util.Date;

/**
 * <p>
 * 系统码表
 * </p>
 *
 * @author zhigang.zhou123
 * @since 2018-11-02
 */
@NoArgsConstructor
@Getter
@Setter
@ToString
@Accessors(chain = true)
@TableName("t_system_code_t")
public class SystemCode extends Model<SystemCode> {

	private static final long serialVersionUID = 1L;

	/**
	 * 主键
	 */
	@TableId(value = "id", type = IdType.AUTO)
	private Integer id;
	/**
	 * 组id（不唯一）
	 */
	private String sysGroupId;
	/**
	 * 该组id下面的key(唯一)
	 */
	private String sysCode;
	/**
	 * 该码表的这个编码对应的值
	 */
	private String sysCodeVal;
	/**
	 * 该码值的说明x信息
	 */
	private String sysCodeDesc;
	/**
	 * 是否启用的状态：1->启用，0->禁用
	 */
	private String status;
	/**
	 * 序号
	 */
	private String orderNum;
	
	/**
	 * 创建时间
	 */
	@DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
	@JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
	private Date createTime;

	@Override
	protected Serializable pkVal() {
		return this.id;
	}

}
