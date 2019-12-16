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
 * 产品化验结果项
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-24
 */
@Data
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
@Accessors(chain = true)
public class ConfigProductTestAttributeIn extends Model<ConfigProductTestAttributeIn> {

    private static final long serialVersionUID = 1L;

    /**
     * 导出类型 1 是文本 2 是图片,3 是函数,10 是数字 默认是文本
     */

    @TableId(value = "id", type = IdType.AUTO)
    @Excel(name = "id", orderNum = "1", type = 10, width = 20)
    private Integer id;
    /**
     * 工厂id
     */
    @Excel(name = "工厂id", orderNum = "2", type = 10, width = 20)
    private Integer factoryId;
    /**
     * 所属企业id
     */
    @Excel(name = "所属企业id", orderNum = "3", type = 10, width = 20)
    private Integer enterpriseId;
    /**
     * 产品id
     */
    @Excel(name = "产品id", orderNum = "4", type = 10, width = 20)
    private Integer productId;
    /**
     * 化验输入项
     */
    @Excel(name = "化验输入项" ,orderNum = "5",width = 20)
    private String assayItemIn;
    /**
     * 输入项标识符
     */
    @Excel(name = "输入项标识符" ,orderNum = "6",width = 20)
    private String markIn;

    /**
     * 是否删除 0未删除 1删除
     */
    @Excel(name = "是否删除 0未删除 1删除", orderNum = "7", type = 10, width = 20)
    private Integer deleted;
    /**
     * 创建人id
     */
    @Excel(name = "创建人id", orderNum = "8", type = 10, width = 20)
    private Integer createId;
    /**
     * 创建人名称
     */
    @Excel(name = "创建人名称" ,orderNum = "9",width = 20)
    private String creator;
    /**
     * 创建时间
     */
    @Excel(name = "创建时间",orderNum = "10",width = 20, format = "yyyy-MM-dd HH:mm:ss")
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private Date createTime;
    /**
     * 修改人id
     */
    @Excel(name = "修改人id", orderNum = "11", type = 10, width = 20)
    private Integer modifyId;
    /**
     * 修改人
     */
    @Excel(name = "修改人" ,orderNum = "12",width = 20)
    private String modifier;
    /**
     * 修改时间
     */
    @Excel(name = "修改时间",orderNum = "13",width = 20, format = "yyyy-MM-dd HH:mm:ss")
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private Date modifyTime;

    /**
     * 化验单位（0 %百分比  1 ‱万分比）
     */
    @Excel(name = "化验单位（0 %百分比  1 ‱万分比）", orderNum = "14", type = 10, width = 20)
    private Integer testUnit;


    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
