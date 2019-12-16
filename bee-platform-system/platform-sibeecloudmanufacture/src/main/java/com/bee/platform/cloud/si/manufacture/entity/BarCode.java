package com.bee.platform.cloud.si.manufacture.entity;

import com.baomidou.mybatisplus.activerecord.Model;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 条形码表
 * </p>
 *
 * @author MP123
 * @since 2019-10-08
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Accessors(chain = true)
public class BarCode extends Model<BarCode> {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.AUTO)
    private Long id;
    /**
     * 编码
     */
    private String code;
    /**
     * 日期
     */
    private String date;
    /**
     * 类型0样品编码1吨袋编码
     */
    private Integer type;
    /**
     * 是否使用0未使用1被使用
     */
    private Integer used;
    /**
     * 数据状态:0删除1正常
     */
    private Integer status;
    /**
     * 备注
     */
    private String remark;


    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
