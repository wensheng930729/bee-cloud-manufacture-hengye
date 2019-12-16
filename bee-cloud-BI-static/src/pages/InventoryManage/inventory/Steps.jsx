import React, { PureComponent, useState, useEffect } from 'react';
import { Form, Row, Col, Card, Radio, Button, InputNumber, TreeSelect, Input, text, message } from 'antd';
import {
  StandardTable,
  CustomModalHOC
} from '@/components/FormWidget';

const { TextArea } = Input;

class Step1 extends PureComponent {

  // state = {
  //   datas: { categoryId: [], inventoryTypeCode: undefined }
  // };

  componentDidMount() {
    const { dispatch } = this.props;
    dispatch({
      type: 'InventoryModel/getInventoryType',
      payload: {},
      callback: (datas) => {
        dispatch({
          type: 'InventoryModel/step1DataChange',
          payload: { ...datas },
        })
      }
    });
  }

  //选中值变化
  onChange = (name, value) => {
    const datas = { ...this.props.datas };
    const { dispatch } = this.props;
    datas[name] = value;
    switch (name) {
      //重置仓库数据
      case 'inventoryTypeCode':
        dispatch({
          type: 'InventoryModel/getinventoryTypeDescService',
          payload: { inventoryTypeCode: value },
          callback: (allChecks) => {
            datas.categoryId = allChecks;
            dispatch({
              type: 'InventoryModel/step1DataChange',
              payload: { ...datas },
            })
          }
        });
        break;
      default:
        dispatch({
          type: 'InventoryModel/step1DataChange',
          payload: { ...datas },
        })
        break;
    }
  };

  render() {
    const {
      datas: { inventoryTypeCode, categoryId }
    } = this.props;
    const { typeDesc = [], types = [] } = this.props;
    const formItemLayout = {
      labelCol: { span: 2 },
      wrapperCol: { span: 6 },
    };

    const rProps = {
      options: types,
      value: inventoryTypeCode,
      onChange: (e) => this.onChange('inventoryTypeCode', e.target.value),
    }

    const tProps = {
      treeData: typeDesc,
      value: categoryId,
      onChange: (value) => this.onChange('categoryId', value),
      treeCheckable: true,
      treeDefaultExpandAll: true,
      // showCheckedStrategy: SHOW_PARENT,
      searchPlaceholder: '请选择',
      style: {
        width: '100%',
      }
    };

    return (
      <Form>
        <Form.Item  {...formItemLayout} label="盘点类型">
          <Radio.Group {...rProps} />
        </Form.Item>
        <Form.Item  {...formItemLayout} label="盘点类型">
          <TreeSelect {...tProps} />
        </Form.Item>
      </Form>
    );

  }
};

//  --------确认入库的modal
const ComfirmForm = props => {


  const {
    productName,
    accountNum,
    differenceNum,
    valueChange,
    actualNum
  } = props;
  const formItemLayout = {
    labelCol: { span: 4 },
    wrapperCol: { span: 16 },
  };

  return (
    <>
      <Form.Item label="产品名称" {...formItemLayout}>
        {productName}
      </Form.Item>
      <Form.Item label="账面数量" {...formItemLayout}>
        {accountNum}吨
      </Form.Item>
      <Form.Item label="差异数量" {...formItemLayout}>
        {differenceNum}吨
      </Form.Item>
      <Form.Item label="实际数量" {...formItemLayout} required>
        <InputNumber
          value={actualNum}
          onChange={value => valueChange(value)}
          min={0}
        />
      </Form.Item>
    </>
  );
};

// const NewComfirmForm = CustomFormHOC(ComfirmForm);

const NewComfirmFormModal = CustomModalHOC(ComfirmForm);


// @connect(({ InventoryModel: { typeDesc, types } }) => ({ typeDesc, types }))(Step2)
const Step2 = props => {

  //调整数量弹窗值
  const [actual, setActual] = useState(props.actualNum);

  let {
    datas,
    datas: { list = [], remarks, inventoryOrderId },
    dispatch
  } = props;

  const formItemLayout = {
    labelCol: { span: 4 },
    wrapperCol: { span: 12 },
  };

  const onChange = (name, value) => {
    const newdatas = { ...datas, [name]: value }
    dispatch({
      type: 'InventoryModel/step2DataChange',
      payload: { ...newdatas },
    })
  }

  //调整弹窗里面 的 值更改
  const valueChange = (value) => {
    let { index, data = {} } = { ...modalValue };
    data.actualNum = value;
    setModalvalue({ data, index })
  }

  const [confirmModalVisible, setConfirmModalVisible] = useState(false);
  const [modalValue, setModalvalue] = useState({});

  const columns = [
    {
      title: '产品名',
      dataIndex: 'productName',
    },
    {
      title: '规格',
      dataIndex: 'productSpecName',
    },
    {
      title: '仓库',
      dataIndex: 'storageName',
    },
    {
      title: '计量单位',
      dataIndex: 'productUnit',
    },
    {
      title: '账面数量',
      dataIndex: 'accountNum',
    },
    {
      title: '实盘数量',
      dataIndex: 'actualNum',
    },
    {
      title: '差异数量',
      dataIndex: 'differenceNum',
    },
    {
      title: '操作',
      dataIndex: 'operation',
      render: (text, record, index) => <a onClick={() => {
        setConfirmModalVisible(true);
        setModalvalue({ data: { ...record }, index });
      }}>调整</a>,
    }
  ];

  const tableNewProps = {
    // onChange: this.handleTableChange,
    loading: false,
    columns,
    data: { list },
    // bordered,
    // scroll,
  };

  //弹出框props
  const ComfirmFormModalProps = {
    visible: confirmModalVisible,
    onOk: a => {
      const { data, index } = modalValue;
      if (data.actualNum===undefined || data.actualNum < 0) {
        return message.error('实际数应该大于等于0')
      }
      const newData = { ...datas }
      let item = newData.list[index];
      newData.list[index] = { ...data, differenceNum: Number((item.accountNum - data.actualNum).toFixed(4)) };
      dispatch({
        type: 'InventoryModel/step2DataChange',
        payload: { ...newData },
      })
      setConfirmModalVisible(false)
    },
    onCancel: () => setConfirmModalVisible(false),
    title: '盘点调整',
    ComProps: {
      ...modalValue.data,
      valueChange
    },
  };
  const inventoryIdProps = {
    disabled: true,
    value: inventoryOrderId
  }
  const remarksProps = {
    value: remarks,
    onChange: (e) => onChange('remarks', e.target.value),
  }

  return (
    <div style={{ marginLeft: 32, marginRight: 32 }}>
      <Card>
        <Row>
          <Col span={13}>
            <Form.Item  {...formItemLayout} label="盘点单单号">
              <Input {...inventoryIdProps} />
            </Form.Item>
          </Col>
          <Col span={13}>
            <Form.Item  {...formItemLayout} label="备注">
              <TextArea {...remarksProps} />
            </Form.Item>
          </Col>
        </Row>
      </Card>

      <Card style={{ marginTop: 10 }}>
        <StandardTable {...tableNewProps} />
        <NewComfirmFormModal {...ComfirmFormModalProps} />
      </Card>
    </div>
  );

}

export { Step1, Step2 };

export default Step2;

// const NewFormSearch = CustomFormHOC(MyFormSearch);
