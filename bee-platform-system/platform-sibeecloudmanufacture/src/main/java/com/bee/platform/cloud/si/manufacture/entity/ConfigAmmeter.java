package com.bee.platform.cloud.si.manufacture.entity;

import cn.afterturn.easypoi.excel.annotation.Excel;
import com.baomidou.mybatisplus.activerecord.Model;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * <p>
 * 电表档案
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
@Data
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
@Accessors(chain = true)
public class ConfigAmmeter extends Model<ConfigAmmeter> {

    private static final long serialVersionUID = 1L;

    /**
     * 导出类型 1 是文本 2 是图片,3 是函数,10 是数字 默认是文本
     */

    @TableId(value = "id", type = IdType.AUTO)
    @Excel(name = "id", orderNum = "1", type = 10, width = 20)
    private Integer id;
    /**
     * 所属企业id
     */
    @Excel(name = "所属企业id", orderNum = "2", type = 10, width = 20)
    private Integer enterpriseId;
    /**
     * 工厂id
     */
    @Excel(name = "工厂id", orderNum = "3", type = 10, width = 20)
    private Integer factoryId;
    /**
     * 电表名称
     */
    @Excel(name = "电表名称" ,orderNum = "4",width = 20)
    private String name;
    /**
     * 电表编号
     */
    @Excel(name = "电表编号" ,orderNum = "5",width = 20)
    private String code;
    /**
     * 状态 1启用 0未启用
     */
    @Excel(name = "状态 1启用 0未启用", orderNum = "6", type = 10, width = 20)
    private Integer status;
    /**
     * 电表类型（0三相三线 1三相四线）
     */
    @Excel(name = "电表类型（0三相三线 1三相四线）", orderNum = "7", type = 10, width = 20)
    private Integer meterType;
    /**
     * 用电类型(0炉变电 1动力电)
     */
    @Excel(name = "用电类型(0炉变电 1动力电)", orderNum = "8", type = 10, width = 20)
    private Integer electricityType;
    /**
     * 协议类型（0 130协议 1 1376.1协议）
     */
    @Excel(name = "协议类型（0 130协议 1 1376.1协议）", orderNum = "9", type = 10, width = 20)
    private Integer protocolType;
    /**
     * sim卡号
     */
    @Excel(name = "sim卡号" ,orderNum = "10",width = 20)
    private String simNumber;
    /**
     * 电压倍率
     */
    @Excel(name = "电压倍率", orderNum = "11", type = 10, width = 20)
    private Integer voltageRate;
    /**
     * 电流倍率
     */
    @Excel(name = "电流倍率", orderNum = "12", type = 10, width = 20)
    private Integer currentRate;
    /**
     * 是否删除 0未删除 1删除
     */
    @Excel(name = "是否删除 0未删除 1删除", orderNum = "13", type = 10, width = 20)
    private Integer deleted;
    /**
     * 创建人id
     */
    @Excel(name = "创建人id", orderNum = "14", type = 10, width = 20)
    private Integer createId;
    /**
     * 创建人名称
     */
    @Excel(name = "创建人名称" ,orderNum = "15",width = 20)
    private String creator;
    /**
     * 创建时间
     */
    @Excel(name = "创建时间",orderNum = "16",width = 20, format = "yyyy-MM-dd HH:mm:ss")
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private Date createTime;
    /**
     * 修改人id
     */
    @Excel(name = "修改人id", orderNum = "17", type = 10, width = 20)
    private Integer modifyId;
    /**
     * 修改人
     */
    @Excel(name = "修改人" ,orderNum = "18",width = 20)
    private String modifier;
    /**
     * 修改时间
     */
    @Excel(name = "修改时间",orderNum = "19",width = 20, format = "yyyy-MM-dd HH:mm:ss")
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private Date modifyTime;


    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
