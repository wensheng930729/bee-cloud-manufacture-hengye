import { Component } from 'react';
import { Button, Card, Table, Divider, Form, Input, InputNumber, Select, Modal, message, Row, Col } from 'antd';
import styles from './index.less';
import withRouter from "umi/withRouter";
import BreadCrumb from '../../../components/BreadCrumb';
import router from 'umi/router';
import { getList, getDetail, getProType, saveLoss, updateLoss, deleteLoss } from './services/index';

const FormItem = Form.Item;
const Option = Select.Option;

//仓库档案文件
@withRouter
@Form.create()
export default class Index extends Component {
  state = {
    types: [],
    data: [],
    currentPage: 1,
    pageSize: 10,
    totalPage: 0,
    totalRecords: 0,
    visible: false,
    type: 0, //0新增，1编辑
    targetObj: null,
    productId: null,
    productName: null
  }

  componentDidMount() {
    const { currentPage, pageSize } = this.state;
    getProType().then(res => {
      if (res.code === 1) {
        this.setState({ types: res.object });
      } else {
        message.error("获取产品类别失败：" + res.message);
      }
    })
    this.getData({ currentPage, pageSize });
  }

  getData = ({ currentPage, pageSize }) => {
    const { getFieldValue } = this.props.form;
    let params = {
      currentPage,
      pageSize,
      productName: getFieldValue("productName") || undefined
    };
    getList(params).then(res => {
      if (res && res.code === 1 && res.object) {
        this.setState({
          data: res.object,
          ...res.page
        })
      } else {
        message.error(res.message);
      }
    })
  }

  onChange = (currentPage) => {
    const { pageSize } = this.state;
    this.getData({ currentPage, pageSize });
  }

  onShowSizeChange = (currentPage, pageSize) => {
    this.getData({ currentPage, pageSize });
  }

  handleAdd = () => {
    const { validateFields, resetFields } = this.props.form;
    const { type, targetObj, productId, productName } = this.state;
    let self = this;
    validateFields(['product', 'loss'], (err, values) => {
      if (!err) {
        let params = {
          id: type ? targetObj.id : undefined,
          loss: values.loss,
          productId,
          productName
        };

        console.log(params);

        if (type) {
          updateLoss(params).then(res => {
            if (res && res.code === 1) {
              message.success(res.message);
              self.setState({ visible: false, type: 0, targetObj: null, productId: null, productName: null });
              self.getData({ currentPage: 1, pageSize: 10 });
              resetFields();
            } else {
              message.error('编辑损耗配置' + res.message);
            }
          })
        } else {
          saveLoss(params).then(res => {
            if (res && res.code === 1) {
              message.success(res.message);
              self.setState({ visible: false, type: 0, targetObj: null, productId: null, productName: null });
              self.getData({ currentPage: 1, pageSize: 10 });
              resetFields();
            } else {
              message.error('新增损耗配置' + res.message);
            }
          })
        }
      }
    });
  }

  handleSearch = () => {
    this.getData({ currentPage: 1, pageSize: 10 });
  }

  handleReset = () => {
    const { resetFields } = this.props.form;
    resetFields();
    this.getData({ currentPage: 1, pageSize: 10 });
  }

  changeObj = (productId, productName) => {
    this.setState({
      productId,
      productName
    })
  }

  handleDelete = (id) => {
    let self = this;
    Modal.confirm({
      title: '您确定要删除吗',
      content: '删除之后无法撤销',
      onOk() {
        deleteLoss(id).then(res => {
          if (res.code === 1) {
            message.success('删除成功:' + res.message);
            self.getData({ currentPage: 1, pageSize: 10 });
          } else {
            message.error('删除失败:' + res.message);
          }
        })
      },
      okText: '确定',
      onCancel() { },
      cancelText: '取消'
    });
  }

  render() {
    const { getFieldDecorator, resetFields } = this.props.form;
    const { types, data, currentPage, pageSize, totalPage, totalRecords, visible, type, targetObj } = this.state;
    const columns = [
      {
        title: '产品名称',
        dataIndex: 'productName',
        key: 'productName',
        width: '25%'
      }, {
        title: '损耗',
        dataIndex: 'loss',
        key: 'loss',
        width: '25%'
      }, {
        title: '创建时间',
        dataIndex: 'createTime',
        key: 'createTime',
        width: '25%'
      }, {
        title: ' 操作',
        key: 'actions',
        render: (text, row) => (
          <div>
            <span onClick={() => this.handleDelete(row.id)} style={{ color: '#1890ff', cursor: 'pointer' }}>删除</span>
            <Divider type="vertical" />
            <span onClick={() => this.setState({ visible: true, type: 1, targetObj: row })} style={{ color: '#1890ff', cursor: 'pointer' }}>编辑</span>
          </div>
        ),
        width: '25%'
      }
    ]

    return (
      <div>
        <BreadCrumb extra={<Button onClick={() => this.setState({ visible: true, type: 0 })} type="primary">新增</Button>} />
        <div className={styles.container}>
          <Card title="原料损耗配置" bordered={false}>
            <Form layout='inline'>
              <Row type="flex" justify="space-between" style={{ width: '100%' }}>
                <Col>
                  <FormItem label="产品名称">
                    {
                      getFieldDecorator('productName')(
                        <Input style={{ width: 260 }} />
                      )
                    }
                  </FormItem>
                </Col>
                <Col>
                  <Button onClick={this.handleReset} style={{ marginRight: 24 }}>重置</Button>
                  <Button type="primary" onClick={this.handleSearch}>查询</Button>
                </Col>
              </Row>
            </Form>
            <Table
              rowKey="id"
              columns={columns}
              dataSource={data}
              pagination={{
                showQuickJumper: true,
                showSizeChanger: true,
                defaultCurrent: 1,
                defaultPageSize: 10,
                current: currentPage,
                pageSize: pageSize,
                total: totalRecords,
                onChange: this.onChange.bind(this),
                pageSizeOptions: ["10", "20", "30"],
                showTotal: (total, range) => `共 ${totalRecords} 条记录 第 ${currentPage} / ${totalPage} 页`,
                onShowSizeChange: this.onShowSizeChange.bind(this)
              }}
            />
          </Card>
        </div>

        <Modal
          title={type ? "编辑原料损耗配置" : "新增原料损耗配置"}
          visible={visible}
          onOk={this.handleAdd}
          okText="保存"
          onCancel={() => { this.setState({ visible: false, type: 0, targetObj: null, productId: null, productName: null }) }}
        >
          <Form layout="inline">
            <FormItem label="产品名称">
              {
                getFieldDecorator('product', {
                  initialValue: type ? targetObj.productId : null,
                  rules: [{ required: true, message: '请选择产品名称' }]
                })(
                  <Select style={{ width: 300 }} onChange={(value, option) => this.changeObj(value, option.props.children)}>
                    {
                      types.map((item, index) => <Option value={item.id} key={item.id}>{item.name}</Option>)
                    }
                  </Select>
                )
              }
            </FormItem>
            <FormItem style={{ marginTop: 24 }} label="原料损耗">
              {
                getFieldDecorator('loss', {
                  initialValue: type ? targetObj.loss : null,
                  rules: [{ required: true, message: '请输入损耗' }]
                })(
                  <InputNumber style={{ width: 300 }} placeholder="请输入" />
                )
              }
            </FormItem>
          </Form>
        </Modal>
      </div>
    )
  }
}