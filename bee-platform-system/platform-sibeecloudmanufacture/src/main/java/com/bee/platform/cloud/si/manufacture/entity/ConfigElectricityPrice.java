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
import java.math.BigDecimal;
import java.util.Date;

/**
 * <p>
 * 
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
@Data
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
@Accessors(chain = true)
public class ConfigElectricityPrice extends Model<ConfigElectricityPrice> {

    private static final long serialVersionUID = 1L;


    /**
     * 导出类型 1 是文本 2 是图片,3 是函数,10 是数字 默认是文本
     */

    /**
     * id
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
     * 用电类型(0炉变电 1动力电)
     */
    @Excel(name = "用电类型(0炉变电 1动力电)", orderNum = "4", type = 10, width = 20)
    private Integer electricityType;
    /**
     * 生效日期
     */
    @Excel(name = "生效日期",orderNum = "5",width = 20, format = "yyyy-MM-dd")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date effectiveDate;
    /**
     * 失效日期
     */
    @Excel(name = "失效日期",orderNum = "6",width = 20, format = "yyyy-MM-dd")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date expirationDate;
    /**
     * 开始时间
     */
    @Excel(name = "开始时间",orderNum = "7",width = 20, format = "HH:mm:ss")
    @JsonFormat(pattern = "HH:mm:ss")
    private Date startTime;
    /**
     * 结束时间
     */
    @Excel(name = "结束时间",orderNum = "8",width = 20, format = "HH:mm:ss")
    @JsonFormat(pattern = "HH:mm:ss")
    private Date endTime;
    /**
     * 电价
     */
    @Excel(name = "电价", orderNum = "9", type = 10, width = 20)
    private BigDecimal price;
    /**
     * 是否删除 0未删除 1删除
     */
    @Excel(name = "是否删除 0未删除 1删除", orderNum = "10", type = 10, width = 20)
    private Integer deleted;
    /**
     * 创建人id
     */
    @Excel(name = "创建人id", orderNum = "11", type = 10, width = 20)
    private Integer createId;
    /**
     * 创建人名称
     */
    @Excel(name = "创建人名称" ,orderNum = "12",width = 20)
    private String creator;
    /**
     * 创建时间
     */
    @Excel(name = "创建时间",orderNum = "13",width = 20, format = "yyyy-MM-dd HH:mm:ss")
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private Date createTime;
    /**
     * 修改人id
     */
    @Excel(name = "修改人id", orderNum = "14", type = 10, width = 20)
    private Integer modifyId;
    /**
     * 修改人名称
     */
    @Excel(name = "修改人名称" ,orderNum = "15",width = 20)
    private String modifier;
    /**
     * 修改时间
     */
    @Excel(name = "修改时间",orderNum = "16",width = 20, format = "yyyy-MM-dd HH:mm:ss")
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private Date modifyTime;


    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
