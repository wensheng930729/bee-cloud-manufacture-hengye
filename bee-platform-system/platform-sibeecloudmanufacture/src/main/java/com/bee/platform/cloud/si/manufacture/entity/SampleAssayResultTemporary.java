package com.bee.platform.cloud.si.manufacture.entity;

import com.baomidou.mybatisplus.activerecord.Model;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * <p>
 * 样品化验结果表临时存储
 * </p>
 *
 * @author MP123
 * @since 2019-12-03
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Accessors(chain = true)
public class SampleAssayResultTemporary extends Model<SampleAssayResultTemporary> {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 样品编号
     */
    private String sampleCode;
    /**
     * 化验项
     */
    private String assayItem;
    /**
     * 化验结果
     */
    private Double assayValue;
    /**
     * 化验单位（0 %百分比  1 ‱万分比）
     */
    private Integer testUnit;
    /**
     * 化验单位名称
     */
    private String unitString;
    /**
     * 业务类型1采购2销售3生产4投料
     */
    private Integer businessType;
    /**
     * 状态0删除1启用
     */
    private Integer status;
    /**
     * 创建人id
     */
    private Integer createId;
    /**
     * 创建人
     */
    private String creator;
    /**
     * 创建时间
     */
    private Date createTime;


    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
