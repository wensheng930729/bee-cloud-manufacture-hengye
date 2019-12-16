import React from 'react';

import { Modal } from 'antd';

import styles from './index.less';

const CustomModalHOC = SelfComponent => {
  class Index extends React.Component {
    static getDerivedStateFromProps(nextProps, curState) {
      // clean state
      if (nextProps.visible !== curState.visible) {
        return {
          visible: nextProps.visible,
        };
      }
      return null;
    }

    state = {
      visible: false,
    };

    handleOk = () => {
      const { onOk } = this.props;
      if (!onOk) {
        return;
      }
      if (
        this.formRef &&
        this.formRef.props &&
        this.formRef.props.form &&
        this.formRef.props.form.validateFields
      ) {
        const { form } = this.formRef.props;
        form.validateFields((err, values) => {
          if (err) {
            return;
          }
          onOk(values);
        });
      } else {
        onOk();
      }
    };

    render() {
      const { visible } = this.state;
      const {
        title,
        onOk,
        onCancel,
        footer,
        width,
        okText,
        ComProps,
        closable,
        ...rest
      } = this.props;

      return (
        <Modal
          {...rest}
          visible={visible}
          title={title}
          onCancel={onCancel}
          onOk={this.handleOk}
          footer={footer}
          okText={okText}
          destroyOnClose
          maskClosable={false}
          width={width}
          closable={closable}
        >
          <div className={styles.modalBodyStyle}>
            <SelfComponent
              wrappedComponentRef={formRef => {
                this.formRef = formRef;
              }}
              {...ComProps}
            />
          </div>
        </Modal>
      );
    }
  }
  return Index;
};

export default CustomModalHOC;
