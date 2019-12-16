package com.bee.platform.cloud.si.manufacture.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.activerecord.Model;
import java.io.Serializable;

import com.baomidou.mybatisplus.annotations.Version;

import lombok.*;
import lombok.experimental.Accessors;

/**
 * <p>
 * 报表字段配置表
 * </p>
 *
 * @author jie.zhang123
 * @since 2019-10-18
 */
@NoArgsConstructor
@Setter
@Getter
@Accessors(chain=true)
@ToString
@TableName("config_report_fields")
public class ConfigReportFields extends Model<ConfigReportFields> {

    private static final long serialVersionUID = -3965593960173055376L;

    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 字段
     */
    private String fields;
    /**
     * 字段中文
     */
    private String fieldsName;
    /**
     * 1, "现存明细表",2, "采购入库表",3, "原料日报表",4, "产成品入库",5, "成品出库", 6, "生产质检", 7, "进出质检",
     * 8, "采购", 9, "销售",10, "产量分析", 11, "合格率", 12, "产量消耗分析", 13, "物流"
     */
    private Integer type;
    /**
     * 状态:1-启用,0-禁用
     */
    private Integer status;
    /**
     * 动态内容衔接标识 1前 2后
     */
    private Integer dynamicContent;


    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
