package com.bee.platform.common.entity;

import com.baomidou.mybatisplus.activerecord.Model;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * <p>
 * 
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-01-08
 */
@Getter
@Setter
@NoArgsConstructor
@Accessors(chain = true)
@TableName("sequence")
public class Sequence extends Model<Sequence> {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.AUTO)
    private Long id;
    /**
     * 序列名称
     */
    private String sequenceKey;
    /**
     * 序列值
     */
    private Integer sequenceValue;
    /**
     * 状态 0 无效 1 有效
     */
    private Integer status;
    /**
     * 创建人id
     */
    private Long createId;
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
