package com.bee.platform.common.entity;

import com.baomidou.mybatisplus.activerecord.Model;
import com.baomidou.mybatisplus.annotations.TableField;
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
 * 公共业务配置表
 * </p>
 *
 * @author zhigang.zhou
 * @since 2018-11-02
 */
@NoArgsConstructor
@Getter
@Setter
@ToString
@Accessors(chain = true)
@TableName("t_config")
public class Config extends Model<Config> {

	private static final long serialVersionUID = 1L;

	/**
	 * 主键
	 */
	@TableId(value = "id", type = IdType.AUTO)
	private Integer id;
	/**
	 * 配置的键
	 */
	@TableField("config_key")
	private String configKey;
	/**
	 * 该键对应的值
	 */
	@TableField("config_value")
	private String configValue;
	/**
	 * 该项配置的描述信息
	 */
	@TableField("config_desc")
	private String configDesc;
	/**
	 * 是否启动该配置：1->启用,0->禁用
	 */
	@TableField("status")
	private String status;
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
