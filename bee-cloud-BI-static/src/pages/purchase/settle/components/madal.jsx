import React, { PureComponent } from 'react';
import { Row, Col } from 'antd';
import { isNumber } from 'lodash';
import { connect } from 'dva';
import styles from './index.less';

import {
  FormItemInput,
  StandardTable,
  CustomModalHOC,
  CustomFormHOC,
} from '@/components/FormWidget';

@connect(({ settleModel: { modalInfo = {} } }) => ({ modalInfo }))
//  --------确认入库的modal弹窗
class ComfirmForm extends PureComponent {
  state = {
    formValue: { unitPriceSettlement: '', weightSettle: '' },
    _amountSettlement: null,
  };

  onValueChange(name, value) {
    let { formValue, _amountSettlement } = this.state;
    formValue[name] = value;
    const { unitPriceSettlement, weightSettle } = formValue;
    if (isNumber(Number(unitPriceSettlement)) && isNumber(Number(weightSettle))) {
      _amountSettlement = (Number(unitPriceSettlement) * Number(weightSettle)).toFixed(2);
    } else {
      _amountSettlement = null;
    }
    this.setState({
      formValue: { unitPriceSettlement, weightSettle },
      _amountSettlement,
    });
  }

  onSelectRow(keys, rows) {
    const {
      onSelectRow,
      form: { setFieldsValue },
    } = this.props;
    const sum = rows.reduce((s, item) => s + item.netWeight, 0);
    this.onValueChange('weightSettle', sum);
    setFieldsValue({ weightSettle: `${sum  }` });
    onSelectRow(keys);
  }

  render() {
    const {
      form: { getFieldDecorator, setFieldsInitialValue },
      rowKey,
      settleStatus,
      modalInfo: {
        settles = [],
        productName,
        supplierName,
        unitPrice,
        unitPriceSettlement,
        weightSettle,
        weightNet,
        weightDeduct,
        amountSettlement,
      },
    } = this.props;

    const columns = [
      {
        title: '序号',
        dataIndex: 'index',
        render: (text, row, index) => index + 1,
      },
      {
        title: '车号',
        dataIndex: 'trainNumber',
      },
      {
        title: '净重(吨)',
        dataIndex: 'netWeight',
      },
      {
        title: '是否折价',
        dataIndex: 'productName',
        render: text => (text ? `¥${text}` : '否'),
      },
      {
        title: '司磅时间',
        dataIndex: 'weighingTime',
      },
    ];
    const formItemLayout = {
      labelCol: { span: 8 },
      wrapperCol: { span: 16 },
    };

    const tableProps = {
      data: { list: settles },
      columns,
      hasRowSelection: settleStatus === '0',
      onSelectRow: this.onSelectRow.bind(this),
      rowKey,
    };

    const { _amountSettlement } = this.state;

    return (
      <>
        <div className={styles.title}>合同信息</div>
        <div className={styles.content}>
          <Row>
            <Col span={8}>
              供应商：<span>{supplierName || '无'}</span>
            </Col>
            <Col span={8}>
              产品名称：<span>{productName || '无'}</span>
            </Col>
            <Col span={8}>
              采购单价（元/吨）：<span>{unitPrice || '无'}</span>
            </Col>
          </Row>
        </div>
        <div className={styles.title}>{settleStatus === '0' ? '结算' : '结算信息'}</div>
        <StandardTable {...tableProps} />
        <div className={styles.content}>
          {settleStatus === '0' ? (
            <Row>
              <Col span={12}>
                <FormItemInput
                  getFieldDecorator={getFieldDecorator}
                  label="结算数量(吨):"
                  fieldId="weightSettle"
                  required
                  formItemLayout={formItemLayout}
                  inputProps={{
                    placeholder: '请输入',
                    type: 'number',
                    readOnly: settleStatus !== '0',
                    onChange: e => this.onValueChange('weightSettle', e.target.value),
                  }}
                  // validator={this.validator.bind(this, '结算数量')}
                />
              </Col>
              <Col span={12}>
                <FormItemInput
                  getFieldDecorator={getFieldDecorator}
                  label="结算单价(元):"
                  fieldId="unitPriceSettlement"
                  required
                  formItemLayout={formItemLayout}
                  // validator={this.validator.bind(this, '结算单价')}
                  inputProps={{
                    placeholder: '请输入',
                    type: 'number',
                    readOnly: settleStatus !== '0',
                    onChange: e => this.onValueChange('unitPriceSettlement', e.target.value),
                  }}
                />
              </Col>
              <Col span={12}>
                结算总价：<span className={styles.red}>{_amountSettlement || '-'}元</span>
              </Col>
            </Row>
          ) : (
            <Row>
              <Col span={12}>
                总净重：<span className={styles.red}>{weightNet}吨</span>
              </Col>
              <Col span={12}>
                结算单价：<span className={styles.red}>{unitPrice}元</span>
              </Col>
              <Col span={12}>
                扣重重量：<span className={styles.red}>{weightDeduct}吨</span>
              </Col>
              <Col span={12}>
                结算总价：<span className={styles.red}>{amountSettlement}元</span>
              </Col>
              <Col span={12}>
                结算重量：<span className={styles.red}>{weightSettle}吨</span>
              </Col>
            </Row>
          )}
        </div>
      </>
    );
  }
}
// const ad=connect(({ settleModel }) => ({ modalInfo: settleModel.modalInfo || {} }))(ComfirmForm)
// 创建表单
const NewComfirmForm = CustomFormHOC(ComfirmForm);

export default CustomModalHOC(NewComfirmForm);
